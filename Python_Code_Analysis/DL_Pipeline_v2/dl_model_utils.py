"""
dl_model_utils.py

Shared model loading utilities for evaluate and predict scripts.
Handles both legacy (dl_04_train.py) and Lightning (dl_04_train_lightning.py) checkpoint formats.
"""

import torch
import torch.nn as nn
from pathlib import Path
from typing import Optional

from dl_03_unet_model import UNet


def load_model_from_checkpoint(
    model_path: Path,
    net: nn.Module,
    device: torch.device,
) -> nn.Module:
    """
    Load weights into an already-constructed network from either checkpoint format.

    Args:
        model_path: Path to .pth or .ckpt checkpoint
        net: Constructed network (UNet)
        device: Target device

    Handles:
        - Legacy format: checkpoint['model_state_dict']
        - Lightning format: checkpoint['state_dict'] with 'net.' prefix
    """
    checkpoint = torch.load(model_path, map_location=device, weights_only=False)

    if 'model_state_dict' in checkpoint:
        # Legacy format from dl_04_train.py
        net.load_state_dict(checkpoint['model_state_dict'])
    elif 'state_dict' in checkpoint:
        # Lightning format — strip the 'net.' prefix
        state = {k.removeprefix('net.'): v
                 for k, v in checkpoint['state_dict'].items()
                 if k.startswith('net.')}
        net.load_state_dict(state)
    else:
        raise ValueError(
            f"Unrecognized checkpoint format in {model_path}. "
            "Expected 'model_state_dict' or 'state_dict' key."
        )

    net = net.to(device)
    net.eval()

    print(f"Loaded model from {model_path}")
    if 'epoch' in checkpoint:
        print(f"  Epoch: {checkpoint['epoch']}")
    if 'val_loss' in checkpoint:
        print(f"  Val loss: {checkpoint['val_loss']:.4f}")

    return net


def load_model(
    model_path: Path,
    device: torch.device,
    in_channels: int,
    num_classes: int,
    base_filters: int = 32,
    depth: int = 4,
    use_aspp: bool = False,
    aspp_rates: tuple = (6, 12, 18),
) -> nn.Module:
    """
    Construct a UNet model and load weights from checkpoint.

    Args:
        model_path: Path to .pth or .ckpt checkpoint
        device: Target device
        in_channels: Number of input channels
        num_classes: Number of output classes
        base_filters: Base filter count
        depth: Network depth
        use_aspp: Whether to add ASPP module at U-Net bottleneck
        aspp_rates: Dilation rates for ASPP branches
    """
    net = UNet(
        in_channels=in_channels,
        num_classes=num_classes,
        base_filters=base_filters,
        depth=depth,
        use_aspp=use_aspp,
        aspp_rates=aspp_rates,
    )
    return load_model_from_checkpoint(model_path, net, device)
