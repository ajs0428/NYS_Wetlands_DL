"""
dl_07_shap_analysis.py

GradientSHAP band-importance analysis for a trained U-Net checkpoint.

Outputs (all prefixed with the model stem so multiple models can be compared):
  {stem}_shap_band_importance.png
  {stem}_shap_band_importance_by_class.png
  {stem}_shap_summary_plot.png
  {stem}_shap_importance.json   (ranked per-band + per-class values)

Usage:
  python dl_07_shap_analysis.py --model Models/best_multiclass_unet.ckpt
  python dl_07_shap_analysis.py --model Models/foo.safetensors --output-dir Models/SHAP
"""

import argparse
import json
import random
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import shap
import torch

from dl_02_dataset import WetlandPatchDataset, create_data_splits
from dl_03_unet_model import get_device
from dl_model_utils import load_model


def read_model_class_info(model_path: Path):
    """
    Return (class_names, num_classes) from a checkpoint's own metadata.

    Prefers the .meta.json sidecar (safetensors exports) and falls back to
    Lightning hyper_parameters in a .ckpt. Returns (None, None) if absent.
    """
    model_path = Path(model_path)

    # .safetensors → sidecar; .ckpt → possible sibling sidecar
    if model_path.suffix == ".safetensors":
        meta_path = model_path.with_suffix(".meta.json")
    else:
        sibling = model_path.with_suffix(".safetensors").with_suffix(".meta.json")
        meta_path = sibling if sibling.exists() else None

    if meta_path is not None and meta_path.exists():
        with open(meta_path) as f:
            meta = json.load(f)
        cn = meta.get("class_names")
        nc = meta.get("num_classes")
        if cn is not None or nc is not None:
            return cn, nc

    if model_path.suffix == ".ckpt":
        ckpt = torch.load(model_path, map_location="cpu", weights_only=False)
        hp = ckpt.get("hyper_parameters", {})
        return hp.get("class_names"), hp.get("num_classes")

    return None, None


class SpatialAvgWrapper(torch.nn.Module):
    """Wrap a segmentation model so GradientExplainer sees (B, C) scalar-per-class outputs."""

    def __init__(self, seg_model):
        super().__init__()
        self.seg_model = seg_model

    def forward(self, x):
        return self.seg_model(x).mean(dim=(-2, -1))


def center_crop(tensor, size):
    if size is None:
        return tensor
    _, h, w = tensor.shape
    y0 = (h - size) // 2
    x0 = (w - size) // 2
    return tensor[:, y0:y0 + size, x0:x0 + size]


def load_tensors(dataset, crop_size):
    return torch.stack([center_crop(dataset[i][0], crop_size) for i in range(len(dataset))])


def build_channel_mapping(predictor_names, normalization, in_channels):
    """Map each expanded model channel back to its source band name."""
    channel_to_band = []
    band_channel_ranges = {}
    ch = 0
    for band_name in predictor_names:
        norm_cfg = normalization[band_name]
        start = ch
        if norm_cfg["method"] == "one_hot":
            n = norm_cfg["num_classes"]
            channel_to_band.extend([band_name] * n)
            ch += n
        else:
            channel_to_band.append(band_name)
            ch += 1
        band_channel_ranges[band_name] = (start, ch)
    assert len(channel_to_band) == in_channels, (
        f"Channel mapping ({len(channel_to_band)}) disagrees with in_channels ({in_channels})"
    )
    return band_channel_ranges


def run_shap(
    model_path: Path,
    patches_dir: Path,
    stats_path: Path,
    output_dir: Path,
    seed: int,
    n_background: int,
    n_test: int,
    crop_size: int,
    base_filters: int,
    depth: int,
    use_aspp: bool,
    aspp_rates,
):
    output_dir.mkdir(parents=True, exist_ok=True)
    stem = model_path.stem

    # Stats: normalization & bands only. Class info comes from the model so that
    # a single stats file can serve binary and multiclass checkpoints (they share
    # identical bands/normalization — only class_names/num_classes differ).
    with open(stats_path) as f:
        stats = json.load(f)
    in_channels = stats["in_channels"]
    predictor_names = stats["predictor_names"]
    normalization = stats["normalization"]

    class_names, num_classes = read_model_class_info(model_path)
    if class_names is None or num_classes is None:
        # Legacy checkpoint without class metadata — fall back to stats.
        class_names = stats["class_names"]
        num_classes = len(class_names)
        print("Warning: class info not found in checkpoint; falling back to stats file.")
    print(f"Classes ({num_classes}): {class_names}")

    # Model (architecture flags are fallbacks; auto-detected when checkpoint has hparams)
    device = get_device()
    model = load_model(
        model_path, device, in_channels, num_classes,
        base_filters=base_filters, depth=depth,
        use_aspp=use_aspp, aspp_rates=tuple(aspp_rates),
    )

    band_channel_ranges = build_channel_mapping(predictor_names, normalization, in_channels)

    # Data
    train_files, _, test_files = create_data_splits(patches_dir, seed=seed)
    random.seed(seed)
    bg_files = random.sample(train_files, min(n_background, len(train_files)))
    test_files = random.sample(test_files, min(n_test, len(test_files)))

    bg_dataset = WetlandPatchDataset(bg_files, stats_path, augment=False)
    test_dataset = WetlandPatchDataset(test_files, stats_path, augment=False)

    print(f"Loading {len(bg_dataset)} background patches...")
    background = load_tensors(bg_dataset, crop_size).to(device)
    print(f"Loading {len(test_dataset)} test patches...")
    test_data = load_tensors(test_dataset, crop_size).to(device)
    print(f"Background: {tuple(background.shape)}  Test: {tuple(test_data.shape)}")

    # SHAP
    wrapped = SpatialAvgWrapper(model).eval()
    explainer = shap.GradientExplainer(wrapped, background)
    print(f"Computing SHAP for {test_data.shape[0]} patches...")
    shap_values = explainer.shap_values(test_data)

    # (N, C, H, W, num_classes) → (num_classes, N, C, H, W)
    sv = np.array(shap_values).transpose(4, 0, 1, 2, 3)
    # Mean |SHAP| over spatial dims: (num_classes, N, C)
    shap_abs = np.abs(sv).mean(axis=(-2, -1))

    n_bands = len(predictor_names)
    shap_band = np.zeros((num_classes, shap_abs.shape[1], n_bands))
    for b_idx, band_name in enumerate(predictor_names):
        s, e = band_channel_ranges[band_name]
        shap_band[:, :, b_idx] = shap_abs[:, :, s:e].sum(axis=-1)

    importance_per_class = shap_band.mean(axis=1)          # (num_classes, n_bands)
    importance_overall = importance_per_class.mean(axis=0)  # (n_bands,)

    sort_idx = np.argsort(importance_overall)[::-1]
    sorted_names = [predictor_names[i] for i in sort_idx]

    # Plot 1: overall importance
    fig, ax = plt.subplots(figsize=(10, 7))
    y_pos = np.arange(len(sorted_names))
    ax.barh(y_pos, importance_overall[sort_idx], color="#3498db", edgecolor="white")
    ax.set_yticks(y_pos)
    ax.set_yticklabels(sorted_names)
    ax.invert_yaxis()
    ax.set_xlabel("Mean |SHAP value|")
    ax.set_title(f"Overall Band Importance — {stem}")
    ax.grid(True, axis="x", alpha=0.3)
    plt.tight_layout()
    p1 = output_dir / f"{stem}_shap_band_importance.png"
    plt.savefig(p1, dpi=150)
    plt.close()
    print(f"Saved {p1}")

    # Plot 2: per-class grouped bars
    palette = ["#2ecc71", "#27ae60", "#3498db", "#9b59b6", "#95a5a6"][:num_classes]
    fig, ax = plt.subplots(figsize=(12, 7))
    bar_width = 0.8 / num_classes
    y_pos = np.arange(n_bands)
    for c_idx, class_name in enumerate(class_names):
        vals = importance_per_class[c_idx][sort_idx]
        offset = (c_idx - num_classes / 2 + 0.5) * bar_width
        ax.barh(y_pos + offset, vals, bar_width,
                label=class_name, color=palette[c_idx], edgecolor="white", linewidth=0.3)
    ax.set_yticks(y_pos)
    ax.set_yticklabels(sorted_names)
    ax.invert_yaxis()
    ax.set_xlabel("Mean |SHAP value|")
    ax.set_title(f"Band Importance by Class — {stem}")
    ax.legend(loc="lower right")
    ax.grid(True, axis="x", alpha=0.3)
    plt.tight_layout()
    p2 = output_dir / f"{stem}_shap_band_importance_by_class.png"
    plt.savefig(p2, dpi=150)
    plt.close()
    print(f"Saved {p2}")

    # Plot 3: summary / beeswarm
    shap_band_mean = shap_band.mean(axis=0)  # (N, n_bands)
    test_np = test_data.cpu().numpy()
    feature_vals = np.zeros((test_np.shape[0], n_bands))
    for b_idx, band_name in enumerate(predictor_names):
        s, e = band_channel_ranges[band_name]
        feature_vals[:, b_idx] = test_np[:, s:e, :, :].mean(axis=(1, 2, 3))

    plt.figure(figsize=(10, 8))
    shap.summary_plot(
        shap_band_mean,
        features=feature_vals,
        feature_names=predictor_names,
        show=False,
        max_display=n_bands,
    )
    plt.title(f"SHAP Summary — {stem}")
    plt.tight_layout()
    p3 = output_dir / f"{stem}_shap_summary_plot.png"
    plt.savefig(p3, dpi=150, bbox_inches="tight")
    plt.close()
    print(f"Saved {p3}")

    # JSON data
    results = {
        "model": str(model_path),
        "model_stem": stem,
        "class_names": class_names,
        "predictor_names": predictor_names,
        "n_background": int(background.shape[0]),
        "n_test": int(test_data.shape[0]),
        "crop_size": crop_size,
        "seed": seed,
        "importance_overall": {
            predictor_names[i]: float(importance_overall[i]) for i in range(n_bands)
        },
        "importance_per_class": {
            class_names[c]: {
                predictor_names[i]: float(importance_per_class[c, i]) for i in range(n_bands)
            }
            for c in range(num_classes)
        },
        "ranking": [predictor_names[i] for i in sort_idx],
    }
    p4 = output_dir / f"{stem}_shap_importance.json"
    with open(p4, "w") as f:
        json.dump(results, f, indent=2)
    print(f"Saved {p4}")

    # Console table
    print(f"\n{'Rank':<6}{'Band':<20}{'Mean |SHAP|':>14}")
    print("-" * 40)
    for rank, i in enumerate(sort_idx, 1):
        print(f"{rank:<6}{predictor_names[i]:<20}{importance_overall[i]:>14.6f}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Compute per-band SHAP importance for a trained model")
    parser.add_argument("--model", type=Path, required=True,
                        help="Path to .ckpt, .safetensors, or .pth checkpoint")
    parser.add_argument("--patches-dir", type=Path, default=Path("Data/Training_Data/R_Patches"))
    parser.add_argument("--stats-path", type=Path, default=Path("Data/Training_Data/normalization_stats.json"))
    parser.add_argument("--output-dir", type=Path, default=Path("Models/SHAP"),
                        help="Directory for plots and JSON (default: Models/SHAP)")
    parser.add_argument("--seed", type=int, default=420,
                        help="Seed for data split (must match training seed to reproduce the test split)")
    parser.add_argument("--n-background", type=int, default=50)
    parser.add_argument("--n-test", type=int, default=20)
    parser.add_argument("--crop-size", type=int, default=128,
                        help="Center-crop size to reduce memory (0 to disable)")
    # Architecture fallbacks (ignored when the checkpoint stores hparams)
    parser.add_argument("--base-filters", type=int, default=32)
    parser.add_argument("--depth", type=int, default=4)
    parser.add_argument("--use-aspp", action="store_true")
    parser.add_argument("--aspp-rates", type=int, nargs="+", default=[6, 12, 18])
    args = parser.parse_args()

    project_root = Path(__file__).parent.parent.parent

    def _abs(p: Path) -> Path:
        return p if p.is_absolute() else project_root / p

    run_shap(
        model_path=_abs(args.model),
        patches_dir=_abs(args.patches_dir),
        stats_path=_abs(args.stats_path),
        output_dir=_abs(args.output_dir),
        seed=args.seed,
        n_background=args.n_background,
        n_test=args.n_test,
        crop_size=args.crop_size if args.crop_size > 0 else None,
        base_filters=args.base_filters,
        depth=args.depth,
        use_aspp=args.use_aspp,
        aspp_rates=args.aspp_rates,
    )
