"""
dl_03_unet3plus_model.py

UNet 3+ architecture for semantic segmentation of wetland classes.

Differs from the plain U-Net (dl_03_unet_model.py) in the decoder: every
decoder node aggregates feature maps from *all* encoder scales and *all*
deeper decoder nodes (full-scale skip connections), instead of a single
same-level skip. Optional deep supervision attaches a classifier head to
every decoder stage and the bottleneck.

Output contract:
  - eval mode (or deep_supervision=False): single tensor (B, num_classes, H, W)
    — keeps dl_05/dl_06 inference unchanged.
  - train mode with deep_supervision=True: list of tensors, each
    (B, num_classes, H, W) upsampled to full resolution. Element 0 is the
    main (finest-decoder) head; the rest are coarse side outputs.

Reuses ConvBlock / EncoderBlock / SqueezeExcitation from dl_03_unet_model so
both architectures share the same residual + SE building blocks.
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
from typing import List, Union

from dl_03_unet_model import ConvBlock, EncoderBlock, SqueezeExcitation, get_device


class UnifyBranch(nn.Module):
    """Bring one source feature map to the target scale and a common width.

    Spatial resampling is done in forward() against an explicit target size
    (robust to odd dimensions): strided max-pool to downsample (exact
    power-of-2 ratio) with adaptive max-pool as the odd-dimension fallback,
    bilinear interpolation to upsample, identity when already matched. Then a
    3x3 conv-BN-ReLU projects to `cat_channels`.

    Note: a plain strided ``F.max_pool2d`` is used for the common exact-ratio
    downsample instead of ``F.adaptive_max_pool2d``. The adaptive variant's
    CUDA backward kernel can raise "invalid configuration argument" (notably
    under AMP / ``16-mixed``); the strided path reuses the same stable kernel
    the encoder's ``nn.MaxPool2d`` uses.
    """

    def __init__(self, in_channels: int, cat_channels: int):
        super().__init__()
        self.conv = nn.Sequential(
            nn.Conv2d(in_channels, cat_channels, kernel_size=3, padding=1, bias=False),
            nn.BatchNorm2d(cat_channels),
            nn.ReLU(inplace=True),
        )

    def forward(self, x: torch.Tensor, target_size) -> torch.Tensor:
        h, w = target_size
        sh, sw = x.shape[-2], x.shape[-1]
        if sh != h or sw != w:
            if sh > h or sw > w:
                # Exact (power-of-2) ratio: strided max-pool, which has a stable
                # CUDA backward kernel. adaptive_max_pool2d backward can crash
                # with "invalid configuration argument" under AMP.
                if sh % h == 0 and sw % w == 0:
                    x = F.max_pool2d(x, kernel_size=(sh // h, sw // w))
                else:
                    x = F.adaptive_max_pool2d(x, (h, w))
            else:
                x = F.interpolate(x, size=(h, w), mode="bilinear", align_corners=False)
        return self.conv(x)


class UNet3Plus(nn.Module):
    """
    UNet 3+ with full-scale skip connections and optional deep supervision.

    Args:
        in_channels: Number of input channels.
        num_classes: Number of output classes.
        base_filters: Filters in the first encoder level (doubles each level).
        depth: Number of encoder downsampling levels (patch size must be
            divisible by 2**depth, as for the plain U-Net).
        cat_channels: Unified channel count per skip branch (paper uses 64).
            Every decoder node is `cat_channels * (depth + 1)` wide.
        deep_supervision: If True, attach a classifier to every decoder stage
            and the bottleneck; forward() returns a list during training.
        dropout: Spatial dropout after the bottleneck.

    Decoder width is constant across stages: cat_channels * (depth + 1).
    For depth=4, cat_channels=64 -> 320-channel decoder nodes (paper setting).
    """

    def __init__(
        self,
        in_channels: int,
        num_classes: int = 4,
        base_filters: int = 32,
        depth: int = 4,
        cat_channels: int = 64,
        deep_supervision: bool = False,
        dropout: float = 0.2,
    ):
        super().__init__()
        self.in_channels = in_channels
        self.num_classes = num_classes
        self.base_filters = base_filters
        self.depth = depth
        self.cat_channels = cat_channels
        self.deep_supervision = deep_supervision

        # Filters per level: encoder skips use filters[0..depth-1],
        # bottleneck uses filters[depth].
        filters = [base_filters * (2 ** i) for i in range(depth + 1)]
        self.filters = filters
        fused_ch = cat_channels * (depth + 1)
        self.fused_ch = fused_ch

        # ── Encoder (shared building blocks with the plain U-Net) ──
        self.encoders = nn.ModuleList()
        in_ch = in_channels
        for i in range(depth):
            self.encoders.append(EncoderBlock(in_ch, filters[i]))
            in_ch = filters[i]

        self.bottleneck = ConvBlock(filters[depth - 1], filters[depth])
        self.bottleneck_dropout = nn.Dropout2d(p=dropout) if dropout > 0 else nn.Identity()

        # ── Decoder: one UnifyBranch per (level i, source stage j) ──
        # A decoder node at level i pulls from every stage j in 0..depth:
        #   j <  i : encoder skip (filters[j]), downsampled
        #   j == i : encoder skip (filters[i]), same scale
        #   i < j < depth : deeper decoder node (fused_ch), upsampled
        #   j == depth : bottleneck (filters[depth]), upsampled
        # unify[i] is a ModuleDict keyed by str(j) for j in 0..depth.
        self.unify = nn.ModuleList()
        self.fuse = nn.ModuleList()
        self.fuse_se = nn.ModuleList()
        # Index i runs over decoder levels depth-1 .. 0; store in that order and
        # remember the mapping so forward() can address by level.
        self._decoder_levels = list(range(depth - 1, -1, -1))
        for i in self._decoder_levels:
            branches = nn.ModuleDict()
            for j in range(depth + 1):
                if j <= i:
                    src_ch = filters[j]          # encoder skip
                elif j < depth:
                    src_ch = fused_ch            # deeper decoder node
                else:
                    src_ch = filters[depth]      # bottleneck
                branches[str(j)] = UnifyBranch(src_ch, cat_channels)
            self.unify.append(branches)
            # Fuse the (depth+1) concatenated branches.
            self.fuse.append(ConvBlock(fused_ch, fused_ch))
            self.fuse_se.append(SqueezeExcitation(fused_ch))

        # ── Output heads ──
        # Main head on the finest decoder node (level 0).
        self.head = nn.Conv2d(fused_ch, num_classes, kernel_size=1)
        if deep_supervision:
            # One side head per coarser decoder level (1..depth-1) + bottleneck.
            self.ds_heads = nn.ModuleDict()
            for i in range(1, depth):
                self.ds_heads[str(i)] = nn.Conv2d(fused_ch, num_classes, kernel_size=1)
            self.ds_heads["bottleneck"] = nn.Conv2d(filters[depth], num_classes, kernel_size=1)

    def forward(self, x: torch.Tensor) -> Union[torch.Tensor, List[torch.Tensor]]:
        full_size = x.shape[-2:]

        # Encoder — collect skips at levels 0..depth-1 (skips[i] at H/2**i).
        skips = []
        for encoder in self.encoders:
            x, skip = encoder(x)
            skips.append(skip)

        bottleneck = self.bottleneck(x)
        bottleneck = self.bottleneck_dropout(bottleneck)

        # Decoder — full-scale aggregation, computed coarse (high i) to fine (i=0).
        decoder = {}  # level -> fused feature map
        for idx, i in enumerate(self._decoder_levels):
            target_size = skips[i].shape[-2:]
            branches = self.unify[idx]
            parts = []
            for j in range(self.depth + 1):
                if j <= i:
                    src = skips[j]               # encoder skip (finer or same)
                elif j < self.depth:
                    src = decoder[j]             # already-computed deeper decoder
                else:
                    src = bottleneck             # coarsest source
                parts.append(branches[str(j)](src, target_size))
            fused = torch.cat(parts, dim=1)
            fused = self.fuse[idx](fused)
            fused = self.fuse_se[idx](fused)
            decoder[i] = fused

        main = self.head(decoder[0])  # already at full resolution

        if self.deep_supervision and self.training:
            outputs = [main]
            for i in range(1, self.depth):
                side = self.ds_heads[str(i)](decoder[i])
                outputs.append(
                    F.interpolate(side, size=full_size, mode="bilinear", align_corners=False)
                )
            bottleneck_side = self.ds_heads["bottleneck"](bottleneck)
            outputs.append(
                F.interpolate(bottleneck_side, size=full_size, mode="bilinear", align_corners=False)
            )
            return outputs

        return main

    def count_parameters(self) -> int:
        """Count total trainable parameters."""
        return sum(p.numel() for p in self.parameters() if p.requires_grad)


def create_unet3plus(
    in_channels: int,
    num_classes: int = 4,
    base_filters: int = 32,
    depth: int = 4,
    cat_channels: int = 64,
    deep_supervision: bool = False,
    dropout: float = 0.2,
    device: torch.device = None,
) -> UNet3Plus:
    """Create and initialize a UNet 3+ model (mirrors create_model in dl_03_unet_model)."""
    if device is None:
        device = get_device()

    model = UNet3Plus(
        in_channels=in_channels,
        num_classes=num_classes,
        base_filters=base_filters,
        depth=depth,
        cat_channels=cat_channels,
        deep_supervision=deep_supervision,
        dropout=dropout,
    ).to(device)

    print("Created UNet3+ model:")
    print(f"  Input channels: {in_channels}")
    print(f"  Output classes: {num_classes}")
    print(f"  Base filters: {base_filters}")
    print(f"  Depth: {depth}")
    print(f"  Cat channels: {cat_channels} (decoder width {model.fused_ch})")
    print(f"  Deep supervision: {deep_supervision}")
    print(f"  Parameters: {model.count_parameters():,}")
    print(f"  Device: {device}")

    return model


if __name__ == "__main__":
    device = get_device()
    print(f"Using device: {device}\n")

    in_channels = 26  # current project: 17 predictors, Geomorph one-hot -> 26 channels

    print("=== UNet3+ (bf=32, depth=4, no deep supervision) ===")
    model = create_unet3plus(in_channels=in_channels, base_filters=32, depth=4, device=device)
    x = torch.randn(2, in_channels, 256, 256, device=device)
    model.eval()
    with torch.no_grad():
        y = model(x)
    print(f"  Input shape:  {x.shape}")
    print(f"  Output shape (eval): {y.shape}\n")

    print("=== UNet3+ (bf=32, depth=4, deep supervision) ===")
    model_ds = create_unet3plus(
        in_channels=in_channels, base_filters=32, depth=4,
        deep_supervision=True, device=device,
    )
    model_ds.train()
    y_list = model_ds(x)
    print(f"  Train outputs: {len(y_list)} heads, shapes {[tuple(o.shape) for o in y_list]}")
    model_ds.eval()
    with torch.no_grad():
        y_eval = model_ds(x)
    print(f"  Eval output: {y_eval.shape} (single tensor)")
