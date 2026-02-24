"""
model_utils.py

Shared model loading utilities for evaluate and predict scripts.
Handles both legacy (04_train.py) and Lightning (04_train_lightning.py) checkpoint formats.
"""

import torch
import torch.nn as nn
from pathlib import Path
from typing import Optional
import importlib.util
import sys


def _import_module(name, path):
    if name in sys.modules:
        return sys.modules[name]
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


_script_dir = Path(__file__).parent
_model = _import_module("unet_model", _script_dir / "03_unet_model.py")
UNet = _model.UNet


def load_model_from_checkpoint(
    model_path: Path,
    net: nn.Module,
    device: torch.device,
) -> nn.Module:
    """
    Load weights into an already-constructed network from either checkpoint format.

    Args:
        model_path: Path to .pth or .ckpt checkpoint
        net: Constructed network (UNet, ResUNet34, etc.)
        device: Target device

    Handles:
        - Legacy format: checkpoint['model_state_dict']
        - Lightning format: checkpoint['state_dict'] with 'net.' prefix
    """
    checkpoint = torch.load(model_path, map_location=device, weights_only=False)

    if 'model_state_dict' in checkpoint:
        # Legacy format from 04_train.py
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
) -> nn.Module:
    """
    Construct a UNet and load weights from checkpoint.

    Drop-in replacement for the load_model() functions previously
    defined locally in 05_evaluate.py and 06_predict.py.
    """
    net = UNet(
        in_channels=in_channels,
        num_classes=num_classes,
        base_filters=base_filters,
        depth=depth,
    )
    return load_model_from_checkpoint(model_path, net, device)
