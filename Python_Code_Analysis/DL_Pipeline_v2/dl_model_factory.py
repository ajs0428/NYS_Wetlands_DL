"""
dl_model_factory.py

Single place that maps an architecture name to a constructed network, so
training and the checkpoint loaders share one dispatch instead of branching
in four places.

Each architecture only receives the keyword arguments it understands; the
others are ignored. `arch` defaults to "unet" everywhere for backward
compatibility with checkpoints that predate architecture tracking.
"""

from typing import Dict, List, Optional

import torch.nn as nn

from dl_03_unet_model import UNet
from dl_03_unet3plus_model import UNet3Plus
from dl_03_mbfusion_model import MBFusionNet

ARCHITECTURES = ("unet", "unet3plus", "mbfusion")


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
    # MBFusionNet-only
    branch_indices: Optional[Dict[str, List[int]]] = None,
    branch_widths: Optional[Dict[str, int]] = None,
    gate_kernel: int = 3,
) -> nn.Module:
    """Construct a network by architecture name.

    Args:
        arch: One of ARCHITECTURES.
        in_channels, num_classes, base_filters, depth, dropout: Shared params.
        use_aspp, aspp_rates: Used only by "unet".
        cat_channels, deep_supervision: Used only by "unet3plus".
        branch_indices, branch_widths, gate_kernel: Used only by "mbfusion";
            branch_indices/branch_widths are REQUIRED there. For mbfusion the
            model DERIVES in_channels from branch_indices (so the two can never
            disagree); the passed in_channels is used only to assert they match.
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
    if arch == "mbfusion":
        if branch_indices is None or branch_widths is None:
            raise ValueError(
                "arch='mbfusion' requires branch_indices and branch_widths. Build them "
                "with dl_experiment_config.branch_indices_from_predictors(stats"
                "['predictor_names']) and branch_widths_for(...); they are also stored "
                "in the checkpoint so eval/predict can reconstruct the model."
            )
        net = MBFusionNet(
            branch_indices=branch_indices,
            branch_widths=branch_widths,
            num_classes=num_classes,
            base_filters=base_filters,
            depth=depth,
            dropout=dropout,
            gate_kernel=gate_kernel,
        )
        if in_channels is not None and net.in_channels != in_channels:
            raise ValueError(
                f"branch_indices span {net.in_channels} channels but in_channels="
                f"{in_channels} was requested -- the branch map does not match the "
                f"config's stats file."
            )
        return net
    raise ValueError(f"Unknown architecture '{arch}'. Expected one of {ARCHITECTURES}.")
