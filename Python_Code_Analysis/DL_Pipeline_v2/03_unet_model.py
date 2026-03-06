"""
03_unet_model.py

U-Net model for semantic segmentation of wetland classes.
Configurable depth and filter counts for local vs HPC training.
"""

import torch
import torch.nn as nn
from typing import List


class ConvBlock(nn.Module):
    """Double convolution block: Conv-BN-ReLU x 2, with optional residual connection."""

    def __init__(self, in_channels: int, out_channels: int, residual: bool = False):
        super().__init__()
        self.conv1 = nn.Sequential(
            nn.Conv2d(in_channels, out_channels, kernel_size=3, padding=1, bias=False),
            nn.BatchNorm2d(out_channels),
            nn.ReLU(inplace=True)
        )
        self.conv2 = nn.Sequential(
            nn.Conv2d(out_channels, out_channels, kernel_size=3, padding=1, bias=False),
            nn.BatchNorm2d(out_channels),
        )
        self.relu = nn.ReLU(inplace=True)

        # Residual shortcut: 1x1 projection when channels differ, identity otherwise
        if residual and in_channels != out_channels:
            self.shortcut = nn.Sequential(
                nn.Conv2d(in_channels, out_channels, kernel_size=1, bias=False),
                nn.BatchNorm2d(out_channels)
            )
        elif residual:
            self.shortcut = nn.Identity()
        else:
            self.shortcut = None

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        out = self.conv1(x)
        out = self.conv2(out)
        if self.shortcut is not None:
            out = out + self.shortcut(x)
        return self.relu(out)


class EncoderBlock(nn.Module):
    """Encoder block: ConvBlock (with residual) + MaxPool"""

    def __init__(self, in_channels: int, out_channels: int):
        super().__init__()
        self.conv = ConvBlock(in_channels, out_channels, residual=True)
        self.pool = nn.MaxPool2d(kernel_size=2, stride=2)

    def forward(self, x: torch.Tensor) -> tuple:
        conv_out = self.conv(x)
        pooled = self.pool(conv_out)
        return pooled, conv_out  # Return both for skip connection


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


class DecoderBlock(nn.Module):
    """Decoder block: Upsample + Concat + ConvBlock + Squeeze-and-Excitation"""

    def __init__(self, in_channels: int, out_channels: int):
        super().__init__()
        self.upsample = nn.ConvTranspose2d(
            in_channels, out_channels, kernel_size=2, stride=2
        )
        self.conv = ConvBlock(in_channels, out_channels)
        self.se = SqueezeExcitation(out_channels)

    def forward(self, x: torch.Tensor, skip: torch.Tensor) -> torch.Tensor:
        x = self.upsample(x)

        # Handle size mismatch from odd dimensions
        if x.shape != skip.shape:
            diff_h = skip.shape[2] - x.shape[2]
            diff_w = skip.shape[3] - x.shape[3]
            x = nn.functional.pad(x, [
                diff_w // 2, diff_w - diff_w // 2,
                diff_h // 2, diff_h - diff_h // 2
            ])

        x = torch.cat([x, skip], dim=1)
        x = self.conv(x)
        return self.se(x)


class UNet(nn.Module):
    """
    U-Net architecture for semantic segmentation.

    Args:
        in_channels: Number of input channels (29 for this project)
        num_classes: Number of output classes (5 for this project)
        base_filters: Number of filters in first layer (doubles each level)
        depth: Number of encoder/decoder levels

    Architecture with depth=4, base_filters=32:
        Encoder: 29 -> 32 -> 64 -> 128 -> 256
        Bottleneck: 256 -> 512
        Decoder: 512 -> 256 -> 128 -> 64 -> 32
        Output: 32 -> 5
    """

    def __init__(
        self,
        in_channels: int,
        num_classes: int = 5,
        base_filters: int = 32,
        depth: int = 4,
        dropout: float = 0.0,
    ):
        super().__init__()

        self.in_channels = in_channels
        self.num_classes = num_classes
        self.base_filters = base_filters
        self.depth = depth

        # Calculate filter sizes for each level
        filters = [base_filters * (2 ** i) for i in range(depth + 1)]

        # Build encoder
        self.encoders = nn.ModuleList()
        in_ch = in_channels
        for i in range(depth):
            self.encoders.append(EncoderBlock(in_ch, filters[i]))
            in_ch = filters[i]

        # Bottleneck
        self.bottleneck = ConvBlock(filters[depth - 1], filters[depth])
        self.bottleneck_dropout = nn.Dropout2d(p=dropout) if dropout > 0 else nn.Identity()

        # Build decoder
        self.decoders = nn.ModuleList()
        for i in range(depth - 1, -1, -1):
            self.decoders.append(DecoderBlock(filters[i + 1], filters[i]))

        # Final output layer
        self.output = nn.Conv2d(filters[0], num_classes, kernel_size=1)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        """
        Forward pass.

        Args:
            x: Input tensor of shape (batch, in_channels, H, W)

        Returns:
            Output tensor of shape (batch, num_classes, H, W)
        """
        # Encoder path - collect skip connections
        skips = []
        for encoder in self.encoders:
            x, skip = encoder(x)
            skips.append(skip)

        # Bottleneck
        x = self.bottleneck(x)
        x = self.bottleneck_dropout(x)

        # Decoder path - use skip connections in reverse order
        for decoder, skip in zip(self.decoders, reversed(skips)):
            x = decoder(x, skip)

        # Output
        return self.output(x)

    def count_parameters(self) -> int:
        """Count total trainable parameters."""
        return sum(p.numel() for p in self.parameters() if p.requires_grad)


def get_device() -> torch.device:
    """
    Auto-detect best available device.

    Returns device in order of preference: cuda > mps > cpu
    """
    if torch.cuda.is_available():
        return torch.device("cuda")
    elif torch.backends.mps.is_available():
        return torch.device("mps")
    else:
        return torch.device("cpu")


def create_model(
    in_channels: int,
    num_classes: int = 5,
    base_filters: int = 32,
    depth: int = 4,
    device: torch.device = None
) -> UNet:
    """
    Create and initialize a U-Net model.

    Args:
        in_channels: Number of input channels
        num_classes: Number of output classes
        base_filters: Base filter count (32 for local, 64 for HPC)
        depth: Network depth (4 for local, 5 for HPC)
        device: Device to place model on (auto-detect if None)

    Returns:
        Initialized UNet model on specified device
    """
    if device is None:
        device = get_device()

    model = UNet(
        in_channels=in_channels,
        num_classes=num_classes,
        base_filters=base_filters,
        depth=depth
    )

    model = model.to(device)

    print(f"Created U-Net model:")
    print(f"  Input channels: {in_channels}")
    print(f"  Output classes: {num_classes}")
    print(f"  Base filters: {base_filters}")
    print(f"  Depth: {depth}")
    print(f"  Parameters: {model.count_parameters():,}")
    print(f"  Device: {device}")

    return model


if __name__ == "__main__":
    # Test the model
    device = get_device()
    print(f"Using device: {device}\n")

    # Local config (29 channels = 20 predictors with 1 one-hot expanding to 10)
    in_channels = 29
    print("=== Local Configuration (M1 Max) ===")
    model_local = create_model(in_channels=in_channels, base_filters=32, depth=4, device=device)

    # Test forward pass
    x = torch.randn(2, in_channels, 128, 128, device=device)
    with torch.no_grad():
        y = model_local(x)
    print(f"  Input shape:  {x.shape}")
    print(f"  Output shape: {y.shape}")

    print("\n=== HPC Configuration (CUDA) ===")
    model_hpc = create_model(in_channels=in_channels, base_filters=64, depth=5, device=device)

    with torch.no_grad():
        y = model_hpc(x)
    print(f"  Input shape:  {x.shape}")
    print(f"  Output shape: {y.shape}")
