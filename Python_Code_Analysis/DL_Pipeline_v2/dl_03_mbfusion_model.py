"""
dl_03_mbfusion_model.py

Multi-branch fusion encoder ("mbfusion") -- the third architecture arm of the
factorial's architecture comparison. Design and rationale live in
arch_fusion/PLAN.md; this module implements Section 3 ("Design A").

Shape of the idea
-----------------
Instead of stacking all predictors into one encoder, each input CATEGORY gets
its own encoder (terrain / lidar / leafon / leafoff), and the branches are fused
at EVERY encoder scale by a per-pixel, softmax-normalized gate. The decoder is
bit-identical to the plain U-Net's, so an mbfusion-vs-unet comparison isolates
"encoder + fusion" as the only changed variable.

    x (B, C, H, W)
      |-- slice by branch_indices --> terrain (17ch) lidar (4) leafon (4) leafoff (4)
      |
      |   each branch: its own EncoderBlock stack at width w_b (w_b * 2**L at level L)
      |
      +-- BranchFusion at each of depth+1 scales:
      |     GroupNorm per branch -> 3x3 gate conv -> softmax ACROSS branches
      |     -> weighted concat -> 1x1 proj to base_filters * 2**L
      |
      +-- shared U-Net decoder (unchanged) -> logits (B, num_classes, H, W)

Why the decoder is untouched: BranchFusion projects each scale to exactly the
width the existing U-Net decoder expects (base_filters * 2**L), so DecoderBlock
is reused verbatim from dl_03_unet_model.

Reused verbatim from dl_03_unet_model: ConvBlock, EncoderBlock, DecoderBlock,
SqueezeExcitation. Only the encoder and the fusion are new.

Cost (bf64 / depth5 / 29ch, measured): ~162M params vs the U-Net's 125M, i.e.
~1.3x total. The branch convolutions alone are ~1.31x a single width-64 encoder;
the six projections add ~12.6M (dominated by the bottleneck's 4608->2048) and
the six gates ~0.33M. Params are NOT the binding constraint -- at level 0 the
fused tensor is 144 channels at 256^2 vs the U-Net's 64 (2.25x the finest-scale
activation), which is what sets the batch size.
"""

from typing import Dict, List, Optional, Sequence

import torch
import torch.nn as nn

from dl_03_unet_model import ConvBlock, EncoderBlock, DecoderBlock


class BranchFusion(nn.Module):
    """Gate and fuse per-branch feature maps at ONE encoder scale.

    Instantiated once per scale with INDEPENDENT (not weight-shared) gates:
    sharing would force a single gating function across all scales and erase the
    scale-resolved result this design exists to produce.

    Args:
        widths:  Per-branch channel counts at this scale, in branch order.
        out_ch:  Output width -- the width the U-Net decoder expects for this
                 scale (base_filters * 2**level).
        gate_kernel: Spatial size of the gate conv (default 3).

    Attributes:
        last_gates: (B, n_branch, H, W) softmax gates from the most recent
            forward pass, detached. Stored rather than returned so forward()'s
            signature stays a plain tensor -> dl_05/dl_06 need no changes.
            Analysis code reads model.fusions[i].last_gates after a forward.
    """

    def __init__(self, widths: Sequence[int], out_ch: int, gate_kernel: int = 3):
        super().__init__()
        widths = list(widths)
        self.widths = widths
        self.n_branch = len(widths)

        # GroupNorm per branch BEFORE gating: removes magnitude confounding from
        # unequal branch widths. Without it a wide branch producing larger
        # features could be compensated by a smaller gate, making the gate maps
        # uninterpretable. num_groups=8 divides every width used here
        # (48*2**L and 32*2**L are all multiples of 8); gcd guards odd configs.
        self.norms = nn.ModuleList(
            nn.GroupNorm(self._groups(w), w) for w in widths
        )

        total = sum(widths)
        # 3x3 (not 1x1) gate: compressing a 144*2**L channel vector to n_branch
        # logits at 1x1 is a high-variance per-pixel decision that yields
        # speckled, hard-to-read gate rasters. 3x3 regularizes it for ~330k
        # params total across all scales. It also matches the ecotone argument --
        # what varies gradually across an FSW/UPL transition is modality
        # RELIABILITY (canopy thins -> return fractions degrade -> leaf-off
        # ground visibility improves). The kernel is fixed in feature-map units,
        # so its ground footprint grows with level (3 m at L0 -> 96 m at L5) as a
        # consequence of downsampling, not as a separate design choice.
        self.gate = nn.Conv2d(total, self.n_branch, gate_kernel,
                              padding=gate_kernel // 2)
        # Weighted CONCATENATION, not weighted sum: concat is what makes unequal
        # branch widths compatible with comparable per-branch gate scalars.
        self.proj = nn.Conv2d(total, out_ch, kernel_size=1)

        self.last_gates: Optional[torch.Tensor] = None

    @staticmethod
    def _groups(width: int, target: int = 8) -> int:
        """Largest divisor of `width` that is <= target (8 for every width here)."""
        for g in range(min(target, width), 0, -1):
            if width % g == 0:
                return g
        return 1

    def forward(self, feats: List[torch.Tensor]) -> torch.Tensor:
        feats = [n(f) for n, f in zip(self.norms, feats)]
        cat = torch.cat(feats, dim=1)
        g = self.gate(cat).softmax(dim=1)          # (B, n_branch, H, W)
        self.last_gates = g.detach()
        fused = torch.cat([f * g[:, i:i + 1] for i, f in enumerate(feats)], dim=1)
        return self.proj(fused)

    def gate_entropy(self) -> Optional[torch.Tensor]:
        """Mean gate entropy (nats) from the last forward, or None.

        Collapse monitor: a healthy gate spreads weight across branches; a
        collapsed one saturates a single branch near 1.0 everywhere and drives
        entropy toward 0. Max is log(n_branch) (~1.386 for 4 branches).
        """
        if self.last_gates is None:
            return None
        g = self.last_gates.clamp_min(1e-8)
        return -(g * g.log()).sum(dim=1).mean()


class MBFusionNet(nn.Module):
    """Multi-branch fusion encoder + shared U-Net decoder.

    Args:
        branch_indices: {branch_name: [channel indices]} into the input tensor,
            in POST-EXPANSION channel space (Geomorph_local occupies 10
            contiguous channels, not 1). Build it with
            dl_experiment_config.branch_indices_from_predictors() from the STATS
            file's predictor_names -- never from raw raster order. A wrong map
            trains fine and reports plausible metrics while feeding each encoder
            the wrong bands, which is why dl_preflight_check [9] gates it.
        branch_widths: {branch_name: level-0 encoder width}. Must cover every key
            in branch_indices.
        num_classes: Output classes.
        base_filters: Decoder/fusion width at level 0 (fused scale L is
            base_filters * 2**L) -- matches the U-Net's base_filters so the
            comparison holds capacity fixed on the decoder side.
        depth: Encoder/decoder levels. There are depth+1 fusion points
            (depth skips + the bottleneck).
        dropout: Dropout2d after the bottleneck fusion (mirrors the U-Net).
        gate_kernel: Gate conv kernel size.

    Note `in_channels` is DERIVED (max index + 1) rather than passed, so it can
    never disagree with branch_indices.
    """

    def __init__(
        self,
        branch_indices: Dict[str, List[int]],
        branch_widths: Dict[str, int],
        num_classes: int = 4,
        base_filters: int = 64,
        depth: int = 5,
        dropout: float = 0.2,
        gate_kernel: int = 3,
    ):
        super().__init__()
        if not branch_indices:
            raise ValueError("branch_indices is empty -- at least one branch required")
        missing = [b for b in branch_indices if b not in branch_widths]
        if missing:
            raise ValueError(f"branch_widths missing entries for branches {missing}")

        # Freeze branch ORDER once: it fixes the gate's channel order, so it must
        # be identical between training and any later gate analysis.
        self.branch_names: List[str] = list(branch_indices)
        self.branch_indices = {b: list(branch_indices[b]) for b in self.branch_names}
        self.branch_widths = {b: int(branch_widths[b]) for b in self.branch_names}
        self.num_classes = num_classes
        self.base_filters = base_filters
        self.depth = depth
        self.gate_kernel = gate_kernel

        flat = [i for b in self.branch_names for i in self.branch_indices[b]]
        if len(flat) != len(set(flat)):
            raise ValueError("branch_indices overlap -- slices must be disjoint")
        self.in_channels = max(flat) + 1
        if sorted(flat) != list(range(self.in_channels)):
            raise ValueError(
                f"branch_indices must cover 0..{self.in_channels - 1} exactly once; "
                f"got {len(flat)} indices covering {len(set(flat))} distinct channels"
            )

        # Index buffers: non-persistent so they follow .to(device) without
        # entering state_dict (which would change checkpoint keys).
        for b in self.branch_names:
            self.register_buffer(f"_idx_{b}",
                                 torch.tensor(self.branch_indices[b], dtype=torch.long),
                                 persistent=False)

        # --- Per-branch encoders -------------------------------------------
        # Branch b at level L is w_b * 2**L wide. Widths are an explicit design
        # knob, NOT proportional to input channel count -- see PLAN Section 2.
        self.encoders = nn.ModuleDict()
        self.bottlenecks = nn.ModuleDict()
        for b in self.branch_names:
            w = self.branch_widths[b]
            blocks = nn.ModuleList()
            in_ch = len(self.branch_indices[b])
            for L in range(depth):
                blocks.append(EncoderBlock(in_ch, w * (2 ** L)))
                in_ch = w * (2 ** L)
            self.encoders[b] = blocks
            self.bottlenecks[b] = ConvBlock(in_ch, w * (2 ** depth))

        # --- Fusion at every scale (depth skips + bottleneck) ---------------
        filters = [base_filters * (2 ** i) for i in range(depth + 1)]
        self.fusions = nn.ModuleList(
            BranchFusion(
                widths=[self.branch_widths[b] * (2 ** L) for b in self.branch_names],
                out_ch=filters[L],
                gate_kernel=gate_kernel,
            )
            for L in range(depth + 1)
        )
        self.bottleneck_dropout = nn.Dropout2d(p=dropout) if dropout > 0 else nn.Identity()

        # --- Shared decoder: identical to dl_03_unet_model.UNet -------------
        self.decoders = nn.ModuleList(
            DecoderBlock(filters[i + 1], filters[i]) for i in range(depth - 1, -1, -1)
        )
        self.output = nn.Conv2d(filters[0], num_classes, kernel_size=1)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        if x.shape[1] != self.in_channels:
            raise ValueError(
                f"expected {self.in_channels} input channels, got {x.shape[1]}"
            )

        # Run each branch's encoder, collecting per-level skips.
        per_branch_skips: Dict[str, List[torch.Tensor]] = {}
        per_branch_bottom: Dict[str, torch.Tensor] = {}
        for b in self.branch_names:
            h = torch.index_select(x, 1, getattr(self, f"_idx_{b}"))
            skips = []
            for block in self.encoders[b]:
                h, skip = block(h)
                skips.append(skip)
            per_branch_skips[b] = skips
            per_branch_bottom[b] = self.bottlenecks[b](h)

        # Fuse each scale -> the widths the U-Net decoder expects.
        fused_skips = [
            self.fusions[L]([per_branch_skips[b][L] for b in self.branch_names])
            for L in range(self.depth)
        ]
        out = self.fusions[self.depth]([per_branch_bottom[b] for b in self.branch_names])
        out = self.bottleneck_dropout(out)

        for decoder, skip in zip(self.decoders, reversed(fused_skips)):
            out = decoder(out, skip)
        return self.output(out)

    # --- Introspection -----------------------------------------------------

    def gate_entropies(self) -> Dict[str, float]:
        """{'gate_entropy/level<L>': mean nats} from the last forward pass.

        Logged per training step so gate collapse is visible early. If a branch
        saturates near 1.0 everywhere in the first few epochs, a temperature on
        the gate logits is the standard fix -- deliberately NOT built in
        speculatively (PLAN Section 3).
        """
        out = {}
        for L, f in enumerate(self.fusions):
            e = f.gate_entropy()
            if e is not None:
                out[f"gate_entropy/level{L}"] = float(e)
        return out

    def gate_maps(self) -> List[torch.Tensor]:
        """Per-scale (B, n_branch, H, W) gates from the last forward pass.

        Interpretation caveat (PLAN Section 6): after gating, `proj` is a 1x1
        conv, so the decoder sees sum_i W_i(f_i * g_i). WITHIN-branch spatial
        comparison is faithful ("terrain reliance rises in depressions"); ACROSS-
        branch absolute comparison is confounded, because a branch with modest
        gates but large W_i can still dominate. Report gate maps standardized
        within branch, and take overall branch importance from SHAP.
        """
        return [f.last_gates for f in self.fusions]

    def count_parameters(self) -> int:
        """Count total trainable parameters."""
        return sum(p.numel() for p in self.parameters() if p.requires_grad)


def create_mbfusion(
    branch_indices: Dict[str, List[int]],
    branch_widths: Dict[str, int],
    num_classes: int = 4,
    base_filters: int = 64,
    depth: int = 5,
    dropout: float = 0.2,
    gate_kernel: int = 3,
    device: Optional[torch.device] = None,
) -> MBFusionNet:
    """Build an MBFusionNet and move it to `device`."""
    model = MBFusionNet(
        branch_indices=branch_indices,
        branch_widths=branch_widths,
        num_classes=num_classes,
        base_filters=base_filters,
        depth=depth,
        dropout=dropout,
        gate_kernel=gate_kernel,
    )
    if device is not None:
        model = model.to(device)
    return model
