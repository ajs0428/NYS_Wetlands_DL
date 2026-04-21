"""
dl_model_utils.py

Shared model loading utilities for evaluate and predict scripts.
Handles three checkpoint formats:
  - safetensors (.safetensors + .meta.json sidecar) — safe, no pickle
  - Lightning (.ckpt) — includes hyper_parameters for auto-detection
  - Legacy (.pth) — requires manual architecture flags
"""

import json
import torch
import torch.nn as nn
from pathlib import Path
from typing import Optional

from dl_03_unet_model import UNet


# ── Internal helpers ──────────────────────────────────────────────────

def _extract_hparams(checkpoint: dict) -> dict:
    """Extract architecture hyperparameters from a Lightning checkpoint."""
    hparams = checkpoint.get("hyper_parameters", {})
    return {
        "in_channels": hparams.get("in_channels"),
        "num_classes": hparams.get("num_classes"),
        "base_filters": hparams.get("base_filters"),
        "depth": hparams.get("depth"),
        "dropout": hparams.get("dropout"),
        "use_aspp": hparams.get("use_aspp"),
        "aspp_rates": hparams.get("aspp_rates"),
    }


def _strip_net_prefix(state_dict: dict) -> dict:
    """Strip 'net.' prefix from Lightning state_dict keys."""
    return {k.removeprefix("net."): v
            for k, v in state_dict.items()
            if k.startswith("net.")}


def _load_weights_into_net(
    checkpoint: dict,
    net: nn.Module,
) -> None:
    """Load weights from a checkpoint dict into a constructed network."""
    if "model_state_dict" in checkpoint:
        # Legacy format from dl_04_train.py
        net.load_state_dict(checkpoint["model_state_dict"])
    elif "state_dict" in checkpoint:
        # Lightning format — strip the 'net.' prefix
        state = _strip_net_prefix(checkpoint["state_dict"])
        net.load_state_dict(state)
    else:
        raise ValueError(
            "Unrecognized checkpoint format. "
            "Expected 'model_state_dict' or 'state_dict' key."
        )


def _print_checkpoint_info(checkpoint: dict, model_path: Path):
    """Print checkpoint metadata."""
    print(f"Loaded model from {model_path}")
    if "epoch" in checkpoint:
        print(f"  Epoch: {checkpoint['epoch']}")
    if "val_loss" in checkpoint:
        print(f"  Val loss: {checkpoint['val_loss']:.4f}")


# ── Safetensors I/O ──────────────────────────────────────────────────

def export_safetensors(
    ckpt_path: Path,
    output_dir: Optional[Path] = None,
    # Manual overrides for legacy checkpoints without hyper_parameters
    in_channels: Optional[int] = None,
    num_classes: Optional[int] = None,
    base_filters: Optional[int] = None,
    depth: Optional[int] = None,
    dropout: Optional[float] = None,
    use_aspp: Optional[bool] = None,
    aspp_rates: Optional[tuple] = None,
) -> Path:
    """
    Export a Lightning .ckpt to .safetensors + .meta.json sidecar.

    Args:
        ckpt_path: Path to the .ckpt file
        output_dir: Directory for output files (default: same as ckpt_path)
        in_channels..aspp_rates: Manual overrides; used when checkpoint
            lacks hyper_parameters (pre-hparam checkpoints)

    Returns:
        Path to the .safetensors file
    """
    from safetensors.torch import save_file as save_safetensors
    from datetime import datetime

    ckpt_path = Path(ckpt_path)
    checkpoint = torch.load(ckpt_path, map_location="cpu", weights_only=False)

    # Extract weights
    if "state_dict" in checkpoint:
        weights = _strip_net_prefix(checkpoint["state_dict"])
    elif "model_state_dict" in checkpoint:
        weights = checkpoint["model_state_dict"]
    else:
        raise ValueError(f"Unrecognized checkpoint format in {ckpt_path}")

    # Build metadata from checkpoint hparams + manual overrides
    hparams = _extract_hparams(checkpoint)
    overrides = {
        "in_channels": in_channels,
        "num_classes": num_classes,
        "base_filters": base_filters,
        "depth": depth,
        "dropout": dropout,
        "use_aspp": use_aspp,
        "aspp_rates": aspp_rates,
    }
    # Manual overrides take precedence
    for key, val in overrides.items():
        if val is not None:
            hparams[key] = val

    # Validate that we have the required architecture params
    missing = [k for k in ("in_channels", "num_classes", "base_filters", "depth")
               if hparams.get(k) is None]
    if missing:
        raise ValueError(
            f"Cannot export: missing architecture params {missing}. "
            f"This checkpoint may predate hparam storage. "
            f"Pass them manually: e.g. --base-filters 64 --depth 5"
        )

    # Ensure aspp_rates is a list for JSON serialization
    if hparams.get("aspp_rates") is not None:
        hparams["aspp_rates"] = list(hparams["aspp_rates"])

    # Set defaults for optional params
    hparams.setdefault("dropout", 0.0)
    hparams.setdefault("use_aspp", False)
    hparams.setdefault("aspp_rates", [6, 12, 18])

    # Also carry over non-architecture metadata if available
    ckpt_hparams = checkpoint.get("hyper_parameters", {})
    for key in ("class_names", "ignore_index", "classification_mode"):
        if key in ckpt_hparams and key not in hparams:
            hparams[key] = ckpt_hparams[key]

    meta = {
        **hparams,
        "source_checkpoint": ckpt_path.name,
        "export_timestamp": datetime.now().isoformat(),
    }
    if "epoch" in checkpoint:
        meta["epoch"] = checkpoint["epoch"]

    # Write files
    out_dir = output_dir or ckpt_path.parent
    stem = ckpt_path.stem
    st_path = out_dir / f"{stem}.safetensors"
    meta_path = out_dir / f"{stem}.meta.json"

    save_safetensors(weights, str(st_path))
    with open(meta_path, "w") as f:
        json.dump(meta, f, indent=2)

    print(f"Exported: {st_path}")
    print(f"Metadata: {meta_path}")
    return st_path


def _load_from_safetensors(
    model_path: Path,
    device: torch.device,
) -> nn.Module:
    """Load a model from .safetensors + .meta.json sidecar."""
    from safetensors.torch import load_file as load_safetensors

    meta_path = model_path.with_suffix(".meta.json")
    if not meta_path.exists():
        raise FileNotFoundError(
            f"Safetensors sidecar not found: {meta_path}. "
            f"The .safetensors file requires a matching .meta.json."
        )

    with open(meta_path) as f:
        meta = json.load(f)

    net = UNet(
        in_channels=meta["in_channels"],
        num_classes=meta["num_classes"],
        base_filters=meta["base_filters"],
        depth=meta["depth"],
        dropout=meta.get("dropout", 0.0),
        use_aspp=meta.get("use_aspp", False),
        aspp_rates=tuple(meta.get("aspp_rates", (6, 12, 18))),
    )

    state_dict = load_safetensors(str(model_path), device=str(device))
    net.load_state_dict(state_dict)
    net = net.to(device)
    net.eval()

    print(f"Loaded model from {model_path} (safetensors)")
    print(f"  Architecture: UNet(in={meta['in_channels']}, bf={meta['base_filters']}, "
          f"depth={meta['depth']}, aspp={meta.get('use_aspp', False)})")
    if "epoch" in meta:
        print(f"  Epoch: {meta['epoch']}")

    return net


# ── Public API ────────────────────────────────────────────────────────

def get_checkpoint_mode(model_path: Path) -> Optional[str]:
    """
    Read classification_mode from a checkpoint without loading weights.

    Returns None if the checkpoint predates mode tracking (legacy .pth or
    older Lightning checkpoints that did not persist the field).
    """
    model_path = Path(model_path)

    # Prefer a sibling .safetensors/.meta.json when present
    if model_path.suffix == ".safetensors":
        meta_path = model_path.with_suffix(".meta.json")
    else:
        st_sibling = model_path.with_suffix(".safetensors")
        meta_path = st_sibling.with_suffix(".meta.json") if st_sibling.exists() else None

    if meta_path is not None and meta_path.exists():
        with open(meta_path) as f:
            return json.load(f).get("classification_mode")

    # Fall back to reading hparams from the .ckpt
    if model_path.suffix == ".ckpt":
        ckpt = torch.load(model_path, map_location="cpu", weights_only=False)
        return ckpt.get("hyper_parameters", {}).get("classification_mode")

    return None


def assert_mode_matches(model_path: Path, stats_mode: str) -> None:
    """
    Raise if the checkpoint's classification_mode disagrees with stats.

    Warns (does not raise) for legacy checkpoints that predate mode tracking.
    """
    ckpt_mode = get_checkpoint_mode(model_path)
    if ckpt_mode is None:
        print(f"Warning: checkpoint does not record classification_mode; "
              f"proceeding on the assumption that it matches stats ({stats_mode}).")
        return
    if ckpt_mode != stats_mode:
        raise ValueError(
            f"Classification mode mismatch: checkpoint was trained with "
            f"'{ckpt_mode}' but normalization_stats.json is '{stats_mode}'. "
            f"Regenerate stats (dl_01) for the correct mode, or use a "
            f"checkpoint trained in '{stats_mode}' mode."
        )


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
    _load_weights_into_net(checkpoint, net)
    net = net.to(device)
    net.eval()
    _print_checkpoint_info(checkpoint, model_path)
    return net


def load_model(
    model_path: Path,
    device: torch.device,
    in_channels: int,
    num_classes: int,
    base_filters: int = 32,
    depth: int = 4,
    dropout: float = 0.0,
    use_aspp: bool = False,
    aspp_rates: tuple = (6, 12, 18),
) -> nn.Module:
    """
    Construct a UNet model and load weights from checkpoint.

    For .safetensors files, architecture is read from the .meta.json sidecar
    and the function arguments are ignored (with a warning if they differ).

    For .ckpt files, architecture is auto-detected from hyper_parameters if
    available, falling back to the function arguments for legacy checkpoints.

    For .pth files, function arguments are always used.

    Args:
        model_path: Path to .safetensors, .ckpt, or .pth checkpoint
        device: Target device
        in_channels: Number of input channels (fallback for legacy checkpoints)
        num_classes: Number of output classes (fallback for legacy checkpoints)
        base_filters: Base filter count (fallback for legacy checkpoints)
        depth: Network depth (fallback for legacy checkpoints)
        dropout: Dropout rate (fallback for legacy checkpoints)
        use_aspp: Whether to add ASPP module (fallback for legacy checkpoints)
        aspp_rates: Dilation rates for ASPP (fallback for legacy checkpoints)
    """
    model_path = Path(model_path)

    # ── Safetensors path: fully self-describing ──
    if model_path.suffix == ".safetensors":
        return _load_from_safetensors(model_path, device)

    # ── Check for sibling .safetensors and prefer it ──
    st_sibling = model_path.with_suffix(".safetensors")
    if st_sibling.exists():
        print(f"Found safetensors export: {st_sibling.name} (using instead of {model_path.name})")
        return _load_from_safetensors(st_sibling, device)

    # ── .ckpt / .pth path ──
    checkpoint = torch.load(model_path, map_location=device, weights_only=False)

    # Try to auto-detect architecture from Lightning hyper_parameters
    if "hyper_parameters" in checkpoint:
        hp = _extract_hparams(checkpoint)
        detected = []
        if hp.get("in_channels") is not None:
            in_channels = hp["in_channels"]
            detected.append(f"in={in_channels}")
        if hp.get("num_classes") is not None:
            num_classes = hp["num_classes"]
            detected.append(f"classes={num_classes}")
        if hp.get("base_filters") is not None:
            base_filters = hp["base_filters"]
            detected.append(f"bf={base_filters}")
        if hp.get("depth") is not None:
            depth = hp["depth"]
            detected.append(f"depth={depth}")
        if hp.get("dropout") is not None:
            dropout = hp["dropout"]
        if hp.get("use_aspp") is not None:
            use_aspp = hp["use_aspp"]
            detected.append(f"aspp={use_aspp}")
        if hp.get("aspp_rates") is not None:
            aspp_rates = tuple(hp["aspp_rates"])
        if detected:
            print(f"Auto-detected architecture from checkpoint: {', '.join(detected)}")

    net = UNet(
        in_channels=in_channels,
        num_classes=num_classes,
        base_filters=base_filters,
        depth=depth,
        dropout=dropout,
        use_aspp=use_aspp,
        aspp_rates=aspp_rates,
    )

    _load_weights_into_net(checkpoint, net)
    net = net.to(device)
    net.eval()
    _print_checkpoint_info(checkpoint, model_path)

    return net


# ── CLI for exporting existing checkpoints ────────────────────────────

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="Export a .ckpt checkpoint to .safetensors + .meta.json"
    )
    parser.add_argument("checkpoint", type=Path,
                        help="Path to .ckpt file to export")
    parser.add_argument("--output-dir", type=Path, default=None,
                        help="Output directory (default: same as checkpoint)")
    parser.add_argument("--in-channels", type=int, default=None,
                        help="Override: number of input channels")
    parser.add_argument("--num-classes", type=int, default=None,
                        help="Override: number of output classes")
    parser.add_argument("--base-filters", type=int, default=None,
                        help="Override: base filter count")
    parser.add_argument("--depth", type=int, default=None,
                        help="Override: network depth")
    parser.add_argument("--dropout", type=float, default=None,
                        help="Override: dropout rate")
    parser.add_argument("--use-aspp", action="store_true", default=None,
                        help="Override: model uses ASPP")
    parser.add_argument("--aspp-rates", type=int, nargs="+", default=None,
                        help="Override: ASPP dilation rates")
    args = parser.parse_args()

    export_safetensors(
        ckpt_path=args.checkpoint,
        output_dir=args.output_dir,
        in_channels=args.in_channels,
        num_classes=args.num_classes,
        base_filters=args.base_filters,
        depth=args.depth,
        dropout=args.dropout,
        use_aspp=args.use_aspp,
        aspp_rates=tuple(args.aspp_rates) if args.aspp_rates else None,
    )
