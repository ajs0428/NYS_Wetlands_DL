"""
dl_03c_dualbranch.py

Dual-branch encoder architecture for wetland segmentation.
Optical/spectral features and terrain/LiDAR features are processed through
separate ResNet encoders with attention-based fusion, following the design
pattern from Jamali & Mahdianpari (2022).

Drop-in replacement for dl_03_unet_model.py -- same nn.Module contract:
    (batch, in_channels, H, W) -> (batch, num_classes, H, W)

The model accepts the full stacked input tensor and splits channels
internally using precomputed index arrays, so no changes are needed
in the dataset, training, evaluation, or prediction scripts.
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
import timm
from typing import List

from dl_03_unet_model import SqueezeExcitation

# ResNet-18 and ResNet-34 produce identical channel widths per stage
RESNET_CHANNELS = [64, 64, 128, 256, 512]


# ── Fusion Modules ───────────────────────────────────────────────────

class GatedFusion(nn.Module):
    """
    Gated cross-modal fusion via squeeze-excitation-style channel attention.
    Learns a soft per-channel routing between optical and terrain features.
    """

    def __init__(self, channels: int):
        super().__init__()
        self.gate = nn.Sequential(
            nn.AdaptiveAvgPool2d(1),
            nn.Flatten(),
            nn.Linear(channels * 2, channels),
            nn.ReLU(inplace=True),
            nn.Linear(channels, channels),
            nn.Sigmoid(),
        )

    def forward(self, optical_feat: torch.Tensor, terrain_feat: torch.Tensor) -> torch.Tensor:
        combined = torch.cat([optical_feat, terrain_feat], dim=1)
        g = self.gate(combined).unsqueeze(-1).unsqueeze(-1)
        return optical_feat * g + terrain_feat * (1 - g)


class ConcatFusion(nn.Module):
    """
    Simple concatenation + 1x1 projection fusion.
    Lower parameter count than gated fusion — useful as an ablation baseline.
    """

    def __init__(self, channels: int):
        super().__init__()
        self.proj = nn.Sequential(
            nn.Conv2d(channels * 2, channels, kernel_size=1, bias=False),
            nn.BatchNorm2d(channels),
            nn.ReLU(inplace=True),
        )

    def forward(self, optical_feat: torch.Tensor, terrain_feat: torch.Tensor) -> torch.Tensor:
        return self.proj(torch.cat([optical_feat, terrain_feat], dim=1))


# ── Decoder ──────────────────────────────────────────────────────────

class DualBranchDecoderBlock(nn.Module):
    """Decoder block: ConvTranspose upsample + skip concat + double conv + SE."""

    def __init__(self, in_channels: int, skip_channels: int, out_channels: int):
        super().__init__()
        self.upsample = nn.ConvTranspose2d(
            in_channels, out_channels, kernel_size=2, stride=2
        )
        self.conv = nn.Sequential(
            nn.Conv2d(out_channels + skip_channels, out_channels, 3, padding=1, bias=False),
            nn.BatchNorm2d(out_channels),
            nn.ReLU(inplace=True),
            nn.Conv2d(out_channels, out_channels, 3, padding=1, bias=False),
            nn.BatchNorm2d(out_channels),
            nn.ReLU(inplace=True),
        )
        self.se = SqueezeExcitation(out_channels)

    def forward(self, x: torch.Tensor, skip: torch.Tensor) -> torch.Tensor:
        x = self.upsample(x)
        # Handle size mismatch from odd dimensions
        if x.shape[2:] != skip.shape[2:]:
            x = F.interpolate(x, size=skip.shape[2:], mode="bilinear", align_corners=False)
        x = torch.cat([x, skip], dim=1)
        x = self.conv(x)
        return self.se(x)


# ── Main Model ───────────────────────────────────────────────────────

class DualBranchUNet(nn.Module):
    """
    Dual-branch U-Net with ResNet encoders and cross-modal fusion.

    - Optical branch (ResNet-34): spectral indices, SAR, NAIP imagery
    - Terrain branch (ResNet-18): DEM derivatives and CHM
    - Fusion at each of 5 encoder stages (gated or concat)
    - Shared decoder with SE attention and skip connections
    - Final upsample to recover original spatial resolution

    Args:
        in_channels: Total input channels (all branches combined).
        num_classes: Number of output classes.
        optical_indices: Channel indices for the optical branch.
        terrain_indices: Channel indices for the terrain branch.
        fusion: Fusion strategy ('gated' or 'concat').
        dropout: Dropout rate after bottleneck (0 = disabled).
        base_filters: Accepted for CLI compatibility, not used.
        depth: Accepted for CLI compatibility, not used.
    """

    def __init__(
        self,
        in_channels: int,
        num_classes: int,
        optical_indices: List[int],
        terrain_indices: List[int],
        fusion: str = "gated",
        dropout: float = 0.0,
        base_filters: int = 64,
        depth: int = 4,
    ):
        super().__init__()
        self.in_channels = in_channels
        self.num_classes = num_classes
        self.fusion_type = fusion

        # Store indices as buffers so they move with the model to GPU/MPS
        self.register_buffer(
            "optical_idx", torch.tensor(optical_indices, dtype=torch.long)
        )
        self.register_buffer(
            "terrain_idx", torch.tensor(terrain_indices, dtype=torch.long)
        )

        optical_ch = len(optical_indices)
        terrain_ch = len(terrain_indices)

        # ── Encoders ─────────────────────────────────────────────────
        self.optical_enc = timm.create_model(
            "resnet34",
            pretrained=False,
            in_chans=optical_ch,
            features_only=True,
            out_indices=(0, 1, 2, 3, 4),
        )
        self.terrain_enc = timm.create_model(
            "resnet18",
            pretrained=False,
            in_chans=terrain_ch,
            features_only=True,
            out_indices=(0, 1, 2, 3, 4),
        )

        # ── Fusion (one module per encoder stage) ────────────────────
        FusionCls = GatedFusion if fusion == "gated" else ConcatFusion
        self.fusions = nn.ModuleList([FusionCls(ch) for ch in RESNET_CHANNELS])

        # ── Bottleneck dropout ───────────────────────────────────────
        self.bottleneck_dropout = (
            nn.Dropout2d(p=dropout) if dropout > 0 else nn.Identity()
        )

        # ── Decoder ──────────────────────────────────────────────────
        # Fused features per stage: [64, 64, 128, 256, 512]
        # Stage 4 (512, stride 32) = bottleneck
        # Skips: stage 3 (256), stage 2 (128), stage 1 (64), stage 0 (64)
        self.decoder_blocks = nn.ModuleList(
            [
                DualBranchDecoderBlock(512, 256, 256),  # stride 32 -> 16
                DualBranchDecoderBlock(256, 128, 128),  # stride 16 -> 8
                DualBranchDecoderBlock(128, 64, 64),    # stride 8  -> 4
                DualBranchDecoderBlock(64, 64, 32),     # stride 4  -> 2
            ]
        )

        # Final head: stride 2 -> stride 1
        self.final_upsample = nn.ConvTranspose2d(32, 32, kernel_size=2, stride=2)
        self.output = nn.Conv2d(32, num_classes, kernel_size=1)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        """
        Forward pass.

        Args:
            x: Input tensor (batch, in_channels, H, W)

        Returns:
            Logits tensor (batch, num_classes, H, W)
        """
        # Split channels into branches
        x_optical = x[:, self.optical_idx]
        x_terrain = x[:, self.terrain_idx]

        # Encode each branch — returns 5 feature maps per branch
        opt_feats = self.optical_enc(x_optical)
        ter_feats = self.terrain_enc(x_terrain)

        # Fuse at each encoder stage
        fused = [self.fusions[i](opt_feats[i], ter_feats[i]) for i in range(5)]

        # Decode: bottleneck is fused[4], skips are fused[3..0]
        x = self.bottleneck_dropout(fused[4])
        for i, decoder in enumerate(self.decoder_blocks):
            x = decoder(x, fused[3 - i])

        # Final upsample to original resolution + classify
        x = self.final_upsample(x)
        return self.output(x)

    def count_parameters(self) -> int:
        """Count total trainable parameters."""
        return sum(p.numel() for p in self.parameters() if p.requires_grad)


# ── Smoke Test ───────────────────────────────────────────────────────

if __name__ == "__main__":
    from dl_03_unet_model import get_device

    device = get_device()
    print(f"Using device: {device}\n")

    # Simulate current band layout: 8 terrain + 10 optical = 18 channels
    terrain_idx = list(range(0, 8))    # DEM + derivatives + CHM
    optical_idx = list(range(8, 18))   # spectral + SAR + NAIP

    for fusion_type in ("gated", "concat"):
        print(f"=== DualBranchUNet (fusion={fusion_type}) ===")
        model = DualBranchUNet(
            in_channels=18,
            num_classes=4,
            optical_indices=optical_idx,
            terrain_indices=terrain_idx,
            fusion=fusion_type,
            dropout=0.2,
        ).to(device)

        print(f"  Optical channels: {len(optical_idx)}")
        print(f"  Terrain channels: {len(terrain_idx)}")
        print(f"  Parameters: {model.count_parameters():,}")

        x = torch.randn(2, 18, 256, 256, device=device)
        with torch.no_grad():
            y = model(x)
        print(f"  Input shape:  {x.shape}")
        print(f"  Output shape: {y.shape}")
        print()
