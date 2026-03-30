"""
dl_05b_evaluate_patches.py

Per-patch evaluation of trained model on test set.
Produces a CSV with per-patch metrics so you can identify which patches
have the largest prediction error and revisit them in GIS.

Output CSV columns:
    patch_file, overall_accuracy, mean_iou, macro_f1,
    <per-class F1 and IoU>, labeled_pixels, unlabeled_pixels
"""

import json
import numpy as np
import pandas as pd
import torch
import torch.nn as nn
from pathlib import Path
from typing import Dict, List, Optional

from dl_02_dataset import create_data_splits, WetlandPatchDataset
from dl_03_unet_model import get_device
from dl_05_evaluate import load_model, compute_confusion_matrix, compute_class_metrics


# ── Per-patch evaluation ─────────────────────────────────────────────

@torch.no_grad()
def evaluate_patches(
    model: nn.Module,
    dataset: WetlandPatchDataset,
    device: torch.device,
    num_classes: int,
    ignore_index: int,
    class_names: List[str],
) -> pd.DataFrame:
    """
    Evaluate every patch individually and return a DataFrame of per-patch metrics.
    """
    model.eval()
    records = []

    for idx in range(len(dataset)):
        X, y = dataset[idx]
        patch_path = dataset.patch_files[idx]

        # Forward pass (add batch dim)
        X_batch = X.unsqueeze(0).to(device)
        preds = model(X_batch).argmax(dim=1).squeeze(0).cpu().numpy()
        targets = y.numpy()

        # Mask out unlabeled pixels
        valid = targets != ignore_index
        n_labeled = int(valid.sum())
        n_unlabeled = int((~valid).sum())

        if n_labeled == 0:
            # Skip patches with no labeled pixels
            records.append({
                "patch_file": patch_path.name,
                "patch_path": str(patch_path),
                "labeled_pixels": 0,
                "unlabeled_pixels": n_unlabeled,
                "overall_accuracy": np.nan,
                "mean_iou": np.nan,
                "macro_f1": np.nan,
            })
            continue

        p = preds[valid]
        t = targets[valid]

        # Confusion matrix and metrics for this patch
        cm = compute_confusion_matrix(p, t, num_classes)
        metrics = compute_class_metrics(cm, class_names)

        row = {
            "patch_file": patch_path.name,
            "patch_path": str(patch_path),
            "labeled_pixels": n_labeled,
            "unlabeled_pixels": n_unlabeled,
            "overall_accuracy": metrics["overall_accuracy"],
            "mean_iou": metrics["mean_iou"],
            "macro_f1": metrics["macro_f1"],
            "macro_precision": metrics["macro_precision"],
            "macro_recall": metrics["macro_recall"],
        }

        # Per-class F1 and IoU columns
        for name in class_names:
            cls = metrics["per_class"][name]
            row[f"{name}_f1"] = cls["f1"]
            row[f"{name}_iou"] = cls["iou"]
            row[f"{name}_support"] = cls["support"]

        records.append(row)

        if (idx + 1) % 50 == 0 or (idx + 1) == len(dataset):
            print(f"  Evaluated {idx + 1}/{len(dataset)} patches")

    return pd.DataFrame(records)


# ── Main ─────────────────────────────────────────────────────────────

def main(
    model_path: Path,
    patches_dir: Path,
    stats_path: Path,
    output_csv: Path,
    base_filters: int = 32,
    depth: int = 4,
    seed: int = 42,
    sort_by: str = "overall_accuracy",
    use_aspp: bool = False,
    aspp_rates: tuple = (6, 12, 18),
):
    """
    Evaluate every test patch individually and save results to CSV.

    Args:
        model_path: Path to trained model checkpoint
        patches_dir: Directory with patches
        stats_path: Path to normalization_stats.json
        output_csv: Path to save results CSV
        base_filters: Model base filters
        depth: Model depth
        seed: Random seed (must match training)
        sort_by: Column to sort results by (ascending = worst first)
        use_aspp: Whether model uses ASPP at bottleneck
        aspp_rates: Dilation rates for ASPP branches
    """
    device = get_device()
    print(f"Using device: {device}")

    # Read configuration from stats
    with open(stats_path) as f:
        stats = json.load(f)
    in_channels = stats["in_channels"]
    num_classes = len(stats["class_names"])
    class_names = stats["class_names"]
    ignore_index = stats.get("ignore_index", 255)

    # Load model
    model = load_model(model_path, device, in_channels, num_classes, base_filters, depth,
                       use_aspp=use_aspp, aspp_rates=aspp_rates)

    # Create test split (no DataLoader — we need per-patch access)
    print("\nCreating test split...")
    _, _, test_files = create_data_splits(patches_dir, seed=seed)
    test_dataset = WetlandPatchDataset(test_files, stats_path, augment=False)
    print(f"Test set: {len(test_dataset)} patches\n")

    # Evaluate each patch
    print("Evaluating patches individually...")
    df = evaluate_patches(model, test_dataset, device, num_classes, ignore_index, class_names)

    # Sort so worst-performing patches are at the top
    df = df.sort_values(sort_by, ascending=True, na_position="first").reset_index(drop=True)

    # Save
    output_csv.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(output_csv, index=False)
    print(f"\nResults saved to {output_csv}")

    # Print summary
    print(f"\n{'='*60}")
    print("Per-Patch Evaluation Summary")
    print(f"{'='*60}")
    print(f"Total test patches: {len(df)}")
    valid_df = df.dropna(subset=["overall_accuracy"])
    print(f"Patches with labeled pixels: {len(valid_df)}")
    print(f"\nOverall accuracy across patches:")
    print(f"  Mean:   {valid_df['overall_accuracy'].mean():.4f}")
    print(f"  Median: {valid_df['overall_accuracy'].median():.4f}")
    print(f"  Min:    {valid_df['overall_accuracy'].min():.4f}")
    print(f"  Max:    {valid_df['overall_accuracy'].max():.4f}")
    print(f"\nMean IoU across patches:")
    print(f"  Mean:   {valid_df['mean_iou'].mean():.4f}")
    print(f"  Median: {valid_df['mean_iou'].median():.4f}")
    print(f"  Min:    {valid_df['mean_iou'].min():.4f}")
    print(f"  Max:    {valid_df['mean_iou'].max():.4f}")

    # Show worst 10
    print(f"\n{'='*60}")
    print(f"Bottom 10 patches (sorted by {sort_by}):")
    print(f"{'='*60}")
    bottom = valid_df.head(10)
    for _, row in bottom.iterrows():
        print(f"  {row['patch_file']:<55} acc={row['overall_accuracy']:.4f}  mIoU={row['mean_iou']:.4f}  F1={row['macro_f1']:.4f}")

    return df


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="Per-patch evaluation of wetland classification model"
    )
    parser.add_argument("--model", type=Path, default=Path("Models/best_model_multiclass.pth"))
    parser.add_argument("--patches-dir", type=Path, default=Path("Data/Training_Data/R_Patches"))
    parser.add_argument("--stats-path", type=Path, default=Path("Data/Training_Data/normalization_stats.json"))
    parser.add_argument("--output-csv", type=Path, default=None,
                        help="Path to save results CSV. Auto-detects binary/multiclass if not specified.")
    parser.add_argument("--base-filters", type=int, default=32)
    parser.add_argument("--depth", type=int, default=4)
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument(
        "--sort-by", type=str, default="overall_accuracy",
        help="Column to sort by ascending (worst first). Options: overall_accuracy, mean_iou, macro_f1"
    )
    parser.add_argument("--use-aspp", action="store_true",
                        help="Model uses ASPP at U-Net bottleneck")
    parser.add_argument("--aspp-rates", type=int, nargs="+", default=[6, 12, 18],
                        help="Dilation rates for ASPP branches (default: 6 12 18)")
    args = parser.parse_args()

    # Handle relative paths
    project_root = Path(__file__).parent.parent.parent
    model_path = project_root / args.model if not args.model.is_absolute() else args.model
    patches_dir = project_root / args.patches_dir if not args.patches_dir.is_absolute() else args.patches_dir
    stats_path = project_root / args.stats_path if not args.stats_path.is_absolute() else args.stats_path

    # Auto-detect binary vs multiclass from stats for output filename
    if args.output_csv is not None:
        output_csv = project_root / args.output_csv if not args.output_csv.is_absolute() else args.output_csv
    else:
        with open(stats_path) as f:
            _stats = json.load(f)
        mode = _stats.get("classification_mode", "multiclass")
        output_csv = project_root / "Data" / "Training_Data" / f"patch_evaluation_{mode}.csv"
        print(f"Detected {mode} classification ({len(_stats['class_names'])} classes)")

    main(
        model_path=model_path,
        patches_dir=patches_dir,
        stats_path=stats_path,
        output_csv=output_csv,
        base_filters=args.base_filters,
        depth=args.depth,
        seed=args.seed,
        sort_by=args.sort_by,
        use_aspp=args.use_aspp,
        aspp_rates=tuple(args.aspp_rates),
    )
