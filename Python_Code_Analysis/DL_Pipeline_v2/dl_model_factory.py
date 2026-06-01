"""
dl_model_factory.py

Single place that maps an architecture name to a constructed network, so
training and the checkpoint loaders share one dispatch instead of branching
in four places.

Each architecture only receives the keyword arguments it understands; the
others are ignored. `arch` defaults to "unet" everywhere for backward
compatibility with checkpoints that predate architecture tracking.
"""

import torch.nn as nn

from dl_03_unet_model import UNet
from dl_03_unet3plus_model import UNet3Plus

ARCHITECTURES = ("unet", "unet3plus")


def build_net(
    arch: str,
    in_channels: int,
    num_classes: int,
    base_filters: int = 32,
    depth: int = 4,
    dropout: float = 0.2,
    # UNet-only
    use_aspp: bool = False,
    aspp_rates: tuple = (6, 12, 18),
    # UNet3Plus-only
    cat_channels: int = 64,
    deep_supervision: bool = False,
) -> nn.Module:
    """Construct a network by architecture name.

    Args:
        arch: One of ARCHITECTURES.
        in_channels, num_classes, base_filters, depth, dropout: Shared params.
        use_aspp, aspp_rates: Used only by "unet".
        cat_channels, deep_supervision: Used only by "unet3plus".
    """
    if arch == "unet":
        return UNet(
            in_channels=in_channels,
            num_classes=num_classes,
            base_filters=base_filters,
            depth=depth,
            dropout=dropout,
            use_aspp=use_aspp,
            aspp_rates=aspp_rates,
        )
    if arch == "unet3plus":
        return UNet3Plus(
            in_channels=in_channels,
            num_classes=num_classes,
            base_filters=base_filters,
            depth=depth,
            dropout=dropout,
            cat_channels=cat_channels,
            deep_supervision=deep_supervision,
        )
    raise ValueError(f"Unknown architecture '{arch}'. Expected one of {ARCHITECTURES}.")
