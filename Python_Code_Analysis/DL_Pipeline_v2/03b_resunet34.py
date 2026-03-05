"""
03b_resunet34.py

ResUNet34 — ResNet-34 encoder with U-Net decoder + SE attention.
No ImageNet pretraining: encoder is randomly initialized to support
arbitrary in_channels (e.g. 35+ multispectral/LiDAR bands).

Usage in 04_train_lightning.py:
    net = ResUNet34(in_channels=in_channels, num_classes=num_classes)
"""

import torch
import torch.nn as nn


class SqueezeExcitation(nn.Module):
    """Squeeze-and-Excitation channel attention block."""

    def __init__(self, channels: int, reduction: int = 16):
        super().__init__()
        mid = max(channels // reduction, 8)
        self.pool = nn.AdaptiveAvgPool2d(1)
        self.fc = nn.Sequential(
            nn.Linear(channels, mid, bias=False),
            nn.ReLU(inplace=True),
            nn.Linear(mid, channels, bias=False),
            nn.Sigmoid()
        )

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        b, c, _, _ = x.shape
        scale = self.pool(x).view(b, c)
        scale = self.fc(scale).view(b, c, 1, 1)
        return x * scale


class BasicBlock(nn.Module):
    """
    ResNet BasicBlock: two 3x3 convs with BN+ReLU and residual shortcut.
    1x1 projection when channels differ or stride != 1.
    """

    def __init__(self, in_channels: int, out_channels: int, stride: int = 1):
        super().__init__()
        self.conv1 = nn.Conv2d(
            in_channels, out_channels, kernel_size=3,
            stride=stride, padding=1, bias=False,
        )
        self.bn1 = nn.BatchNorm2d(out_channels)
        self.relu = nn.ReLU(inplace=True)
        self.conv2 = nn.Conv2d(
            out_channels, out_channels, kernel_size=3,
            stride=1, padding=1, bias=False,
        )
        self.bn2 = nn.BatchNorm2d(out_channels)

        if stride != 1 or in_channels != out_channels:
            self.shortcut = nn.Sequential(
                nn.Conv2d(in_channels, out_channels, kernel_size=1,
                          stride=stride, bias=False),
                nn.BatchNorm2d(out_channels),
            )
        else:
            self.shortcut = nn.Identity()

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        out = self.relu(self.bn1(self.conv1(x)))
        out = self.bn2(self.conv2(out))
        out = out + self.shortcut(x)
        return self.relu(out)


def _make_stage(in_channels: int, out_channels: int, num_blocks: int,
                stride: int = 1) -> nn.Sequential:
    """Build a sequence of BasicBlocks (first block may downsample)."""
    blocks = [BasicBlock(in_channels, out_channels, stride=stride)]
    for _ in range(1, num_blocks):
        blocks.append(BasicBlock(out_channels, out_channels, stride=1))
    return nn.Sequential(*blocks)


class DecoderBlock(nn.Module):
    """Decoder block: ConvTranspose upsample + skip concat + double conv + SE."""

    def __init__(self, in_channels: int, skip_channels: int, out_channels: int):
        super().__init__()
        self.upsample = nn.ConvTranspose2d(
            in_channels, out_channels, kernel_size=2, stride=2,
        )
        self.conv = nn.Sequential(
            nn.Conv2d(out_channels + skip_channels, out_channels,
                      kernel_size=3, padding=1, bias=False),
            nn.BatchNorm2d(out_channels),
            nn.ReLU(inplace=True),
            nn.Conv2d(out_channels, out_channels,
                      kernel_size=3, padding=1, bias=False),
            nn.BatchNorm2d(out_channels),
            nn.ReLU(inplace=True),
        )
        self.se = SqueezeExcitation(out_channels)

    def forward(self, x: torch.Tensor, skip: torch.Tensor) -> torch.Tensor:
        x = self.upsample(x)
        # Handle size mismatch from odd dimensions
        if x.shape[2:] != skip.shape[2:]:
            diff_h = skip.shape[2] - x.shape[2]
            diff_w = skip.shape[3] - x.shape[3]
            x = nn.functional.pad(x, [
                diff_w // 2, diff_w - diff_w // 2,
                diff_h // 2, diff_h - diff_h // 2,
            ])
        x = torch.cat([x, skip], dim=1)
        x = self.conv(x)
        return self.se(x)


class ResUNet34(nn.Module):
    """
    U-Net with ResNet-34 encoder (from scratch, no pretrained weights).

    Encoder layout (ResNet-34 block counts [3, 4, 6, 3]):
        Stem:    in_channels -> bf   (7x7 conv, BN, ReLU)  — keeps spatial size
        Pool:    bf -> bf            (3x3 maxpool stride 2) — /2
        Stage 1: bf -> bf            (3 BasicBlocks)        — /2 (same)
        Stage 2: bf -> 2*bf          (4 BasicBlocks)        — /4
        Stage 3: 2*bf -> 4*bf        (6 BasicBlocks)        — /8
        Stage 4: 4*bf -> 8*bf        (3 BasicBlocks)        — /16 (bottleneck)

    Decoder (4 upsample stages):
        Up4: 8*bf -> 4*bf,  skip from stage 3
        Up3: 4*bf -> 2*bf,  skip from stage 2
        Up2: 2*bf -> bf,    skip from stage 1
        Up1: bf   -> bf,    skip from stem (before maxpool)

    Head: 1x1 conv -> num_classes

    Interface matches UNet: (B, in_channels, H, W) -> (B, num_classes, H, W)
    Input spatial dimensions must be divisible by 16.

    Args:
        in_channels: Number of input bands. No default — must be provided.
        num_classes: Number of output segmentation classes.
        base_filters: Base filter count (default 64 mirrors ResNet convention).
    """

    def __init__(self, in_channels: int, num_classes: int, base_filters: int = 64):
        super().__init__()
        bf = base_filters

        # Stem: 7x7 conv (preserves spatial size with padding=3)
        self.stem = nn.Sequential(
            nn.Conv2d(in_channels, bf, kernel_size=7, stride=1, padding=3, bias=False),
            nn.BatchNorm2d(bf),
            nn.ReLU(inplace=True),
        )
        # MaxPool to downsample /2
        self.pool = nn.MaxPool2d(kernel_size=3, stride=2, padding=1)

        # Encoder stages (ResNet-34 config: [3, 4, 6, 3])
        self.stage1 = _make_stage(bf, bf, num_blocks=3, stride=1)       # /2 (from pool)
        self.stage2 = _make_stage(bf, 2 * bf, num_blocks=4, stride=2)   # /4
        self.stage3 = _make_stage(2 * bf, 4 * bf, num_blocks=6, stride=2)  # /8
        self.stage4 = _make_stage(4 * bf, 8 * bf, num_blocks=3, stride=2)  # /16

        # Decoder
        self.up4 = DecoderBlock(8 * bf, 4 * bf, 4 * bf)   # skip from stage3
        self.up3 = DecoderBlock(4 * bf, 2 * bf, 2 * bf)   # skip from stage2
        self.up2 = DecoderBlock(2 * bf, bf, bf)            # skip from stage1
        self.up1 = DecoderBlock(bf, bf, bf)                # skip from stem

        # Classification head
        self.head = nn.Conv2d(bf, num_classes, kernel_size=1)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        # Encoder
        s0 = self.stem(x)          # (B, bf, H, W)       — skip for up1
        p0 = self.pool(s0)         # (B, bf, H/2, W/2)
        s1 = self.stage1(p0)       # (B, bf, H/2, W/2)   — skip for up2
        s2 = self.stage2(s1)       # (B, 2bf, H/4, W/4)  — skip for up3
        s3 = self.stage3(s2)       # (B, 4bf, H/8, W/8)  — skip for up4
        s4 = self.stage4(s3)       # (B, 8bf, H/16, W/16) — bottleneck

        # Decoder
        x = self.up4(s4, s3)       # (B, 4bf, H/8, W/8)
        x = self.up3(x, s2)        # (B, 2bf, H/4, W/4)
        x = self.up2(x, s1)        # (B, bf, H/2, W/2)
        x = self.up1(x, s0)        # (B, bf, H, W)

        return self.head(x)        # (B, num_classes, H, W)

    def count_parameters(self) -> int:
        """Count total trainable parameters."""
        return sum(p.numel() for p in self.parameters() if p.requires_grad)


if __name__ == "__main__":
    # Self-test with random tensor
    device = torch.device("cuda" if torch.cuda.is_available()
                          else "mps" if torch.backends.mps.is_available()
                          else "cpu")
    print(f"Using device: {device}\n")

    in_channels = 35
    num_classes = 5

    print("=== ResUNet34 (base_filters=64) ===")
    model = ResUNet34(in_channels=in_channels, num_classes=num_classes, base_filters=64)
    model = model.to(device)
    print(f"  Parameters: {model.count_parameters():,}")

    x = torch.randn(2, in_channels, 256, 256, device=device)
    with torch.no_grad():
        y = model(x)
    print(f"  Input shape:  {x.shape}")
    print(f"  Output shape: {y.shape}")
    assert y.shape == (2, num_classes, 256, 256), f"Shape mismatch: {y.shape}"
    print("  PASS")

    print("\n=== ResUNet34 (base_filters=32) ===")
    model32 = ResUNet34(in_channels=in_channels, num_classes=num_classes, base_filters=32)
    model32 = model32.to(device)
    print(f"  Parameters: {model32.count_parameters():,}")

    with torch.no_grad():
        y32 = model32(x)
    print(f"  Input shape:  {x.shape}")
    print(f"  Output shape: {y32.shape}")
    assert y32.shape == (2, num_classes, 256, 256), f"Shape mismatch: {y32.shape}"
    print("  PASS")
