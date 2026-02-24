"""
03b_resunet34.py

ResUNet34 — ResNet-34 encoder with U-Net decoder.
Scaffold for future architecture experiments.

No ImageNet pretraining: encoder is randomly initialized to support
arbitrary in_channels (e.g. 35+ multispectral/LiDAR bands).

Usage in 04_train_lightning.py:
    # Replace the UNet line with:
    from 03b_resunet34 import ResUNet34  # (via importlib pattern)
    net = ResUNet34(in_channels=in_channels, num_classes=num_classes)
"""

import torch
import torch.nn as nn


class ResUNet34(nn.Module):
    """
    U-Net with ResNet-34 encoder (from scratch, no pretrained weights).

    Interface matches UNet: (B, in_channels, H, W) -> (B, num_classes, H, W)

    Args:
        in_channels: Number of input bands. No default — must be provided.
        num_classes: Number of output segmentation classes.
        base_filters: Base filter count (default 64 mirrors ResNet convention).
    """

    def __init__(self, in_channels: int, num_classes: int, base_filters: int = 64):
        super().__init__()
        raise NotImplementedError(
            "ResUNet34 architecture not yet implemented. "
            "This scaffold defines the expected interface — fill in the "
            "encoder/decoder when ready to experiment."
        )

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        raise NotImplementedError

    def count_parameters(self) -> int:
        return sum(p.numel() for p in self.parameters() if p.requires_grad)
