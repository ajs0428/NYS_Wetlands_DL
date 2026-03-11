"""
dl_05_evaluate.py

Evaluate trained model on test set.
Computes per-class metrics, confusion matrix, and overall performance.
"""

import json
import numpy as np
import torch
import torch.nn as nn
from pathlib import Path
from typing import Dict, List, Optional

from dl_02_dataset import create_dataloaders
from dl_03_unet_model import get_device
from dl_band_utils import load_band_config, get_branch_indices
from dl_model_utils import load_model


def compute_confusion_matrix(
    preds: np.ndarray,
    targets: np.ndarray,
    num_classes: int
) -> np.ndarray:
    """
    Compute confusion matrix.

    Args:
        preds: Predicted class labels (flattened)
        targets: Ground truth labels (flattened)
        num_classes: Number of classes

    Returns:
        Confusion matrix of shape (num_classes, num_classes)
        where cm[i, j] = count of pixels with true class i predicted as class j
    """
    cm = np.zeros((num_classes, num_classes), dtype=np.int64)
    valid = (targets >= 0) & (targets < num_classes) & (preds >= 0) & (preds < num_classes)
    np.add.at(cm, (targets[valid], preds[valid]), 1)
    return cm


def compute_class_metrics(cm: np.ndarray, class_names: List[str]) -> Dict:
    """
    Compute per-class precision, recall, F1, and IoU from confusion matrix.

    Args:
        cm: Confusion matrix
        class_names: List of class names

    Returns:
        Dictionary with per-class and aggregate metrics
    """
    num_classes = len(class_names)
    metrics = {"per_class": {}}

    precisions = []
    recalls = []
    f1s = []
    ious = []

    for i, name in enumerate(class_names):
        tp = cm[i, i]
        fp = cm[:, i].sum() - tp
        fn = cm[i, :].sum() - tp

        precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0
        recall = tp / (tp + fn) if (tp + fn) > 0 else 0.0
        f1 = 2 * precision * recall / (precision + recall) if (precision + recall) > 0 else 0.0
        iou = tp / (tp + fp + fn) if (tp + fp + fn) > 0 else 0.0

        metrics["per_class"][name] = {
            "precision": round(precision, 4),
            "recall": round(recall, 4),
            "f1": round(f1, 4),
            "iou": round(iou, 4),
            "support": int(cm[i, :].sum())
        }

        precisions.append(precision)
        recalls.append(recall)
        f1s.append(f1)
        ious.append(iou)

    # Aggregate metrics
    metrics["macro_precision"] = round(np.mean(precisions), 4)
    metrics["macro_recall"] = round(np.mean(recalls), 4)
    metrics["macro_f1"] = round(np.mean(f1s), 4)
    metrics["mean_iou"] = round(np.mean(ious), 4)
    metrics["overall_accuracy"] = round(np.diag(cm).sum() / cm.sum(), 4)

    return metrics


@torch.no_grad()
def evaluate(
    model: nn.Module,
    loader,
    device: torch.device,
    num_classes: int,
    ignore_index: int,
    class_names: List[str]
) -> Dict:
    """
    Evaluate model on a dataset.

    Args:
        model: Trained model
        loader: DataLoader for evaluation
        device: Computation device
        num_classes: Number of classes
        ignore_index: Label value to exclude
        class_names: List of class names

    Returns:
        Dictionary with evaluation metrics
    """
    model.eval()

    all_preds = []
    all_targets = []

    print("Running inference...")
    for batch_idx, (X, y) in enumerate(loader):
        X = X.to(device)
        outputs = model(X)
        preds = outputs.argmax(dim=1).cpu().numpy()

        all_preds.append(preds.flatten())
        all_targets.append(y.numpy().flatten())

        if (batch_idx + 1) % 20 == 0:
            print(f"  Processed batch {batch_idx + 1}/{len(loader)}")

    all_preds = np.concatenate(all_preds)
    all_targets = np.concatenate(all_targets)

    # Filter out unlabeled pixels (ignore_index)
    valid = all_targets != ignore_index
    print(f"  Total pixels: {len(all_targets):,}, Labeled: {valid.sum():,}, "
          f"Unlabeled: {(~valid).sum():,}")
    all_preds = all_preds[valid]
    all_targets = all_targets[valid]

    print("Computing metrics...")
    cm = compute_confusion_matrix(all_preds, all_targets, num_classes)
    metrics = compute_class_metrics(cm, class_names)
    metrics["confusion_matrix"] = cm.tolist()

    return metrics


def print_metrics(metrics: Dict, class_names: List[str]):
    """Pretty print evaluation metrics."""
    print("\n" + "=" * 60)
    print("Evaluation Results")
    print("=" * 60)

    print(f"\nOverall Accuracy: {metrics['overall_accuracy']:.4f}")
    print(f"Mean IoU: {metrics['mean_iou']:.4f}")
    print(f"Macro F1: {metrics['macro_f1']:.4f}")

    print("\nPer-Class Metrics:")
    print("-" * 60)
    print(f"{'Class':<12} {'Precision':>10} {'Recall':>10} {'F1':>10} {'IoU':>10} {'Support':>10}")
    print("-" * 60)

    for name in class_names:
        m = metrics["per_class"][name]
        print(f"{name:<12} {m['precision']:>10.4f} {m['recall']:>10.4f} "
              f"{m['f1']:>10.4f} {m['iou']:>10.4f} {m['support']:>10,}")

    print("-" * 60)

    # Print confusion matrix
    print("\nConfusion Matrix:")
    cm = np.array(metrics["confusion_matrix"])
    print(f"{'':>12}", end="")
    for name in class_names:
        print(f"{name[:6]:>8}", end="")
    print()

    for i, name in enumerate(class_names):
        print(f"{name:<12}", end="")
        for j in range(len(class_names)):
            print(f"{cm[i, j]:>8,}", end="")
        print()


def main(
    model_path: Path,
    patches_dir: Path,
    stats_path: Path,
    output_path: Optional[Path] = None,
    batch_size: int = 16,
    base_filters: int = 32,
    depth: int = 4,
    seed: int = 42,
    architecture: str = "unet",
    fusion: str = "gated",
):
    """
    Main evaluation function.

    Args:
        model_path: Path to trained model checkpoint
        patches_dir: Directory with patches
        stats_path: Path to normalization stats
        output_path: Optional path to save metrics JSON
        batch_size: Batch size for evaluation
        base_filters: Model base filters
        depth: Model depth
        seed: Random seed (must match training)
        architecture: Model architecture
        fusion: Dual-branch fusion strategy (only used with dualbranch)
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

    # Compute branch indices for dual-branch architecture
    optical_idx = terrain_idx = None
    if architecture == "dualbranch":
        config = load_band_config()
        optical_idx, terrain_idx = get_branch_indices(stats, config)

    # Load model
    model = load_model(model_path, device, in_channels, num_classes, base_filters, depth,
                       architecture=architecture, optical_indices=optical_idx,
                       terrain_indices=terrain_idx, fusion=fusion)

    # Create test loader
    print("\nLoading test data...")
    _, _, test_loader, _ = create_dataloaders(
        patches_dir, stats_path,
        batch_size=batch_size,
        num_workers=4,
        seed=seed,
    )

    # Evaluate
    metrics = evaluate(model, test_loader, device, num_classes, ignore_index, class_names)
    print_metrics(metrics, class_names)

    # Save results
    if output_path:
        output_path.parent.mkdir(parents=True, exist_ok=True)
        with open(output_path, 'w') as f:
            json.dump(metrics, f, indent=2)
        print(f"\nMetrics saved to {output_path}")

    return metrics


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Evaluate wetland classification model")
    parser.add_argument("--model", type=Path, default=Path("Models/best_model.pth"))
    parser.add_argument("--patches-dir", type=Path, default=Path("Data/Training_Data/R_Patches"))
    parser.add_argument("--stats-path", type=Path, default=Path("Data/Training_Data/normalization_stats.json"))
    parser.add_argument("--output", type=Path, default=None)
    parser.add_argument("--batch-size", type=int, default=16)
    parser.add_argument("--base-filters", type=int, default=32)
    parser.add_argument("--depth", type=int, default=4)
    parser.add_argument("--seed", type=int, required=True,
                        help="Random seed (must match training seed for correct test split)")
    parser.add_argument("--architecture", type=str, default="unet",
                        choices=["unet", "resunet34", "dualbranch"],
                        help="Model architecture (default: unet)")
    parser.add_argument("--fusion", type=str, default="gated",
                        choices=["gated", "concat"],
                        help="Dual-branch fusion strategy (default: gated)")
    args = parser.parse_args()

    # Handle relative paths
    project_root = Path(__file__).parent.parent.parent
    model_path = project_root / args.model if not args.model.is_absolute() else args.model
    patches_dir = project_root / args.patches_dir if not args.patches_dir.is_absolute() else args.patches_dir
    stats_path = project_root / args.stats_path if not args.stats_path.is_absolute() else args.stats_path
    output_path = project_root / args.output if args.output and not args.output.is_absolute() else args.output

    main(
        model_path=model_path,
        patches_dir=patches_dir,
        stats_path=stats_path,
        output_path=output_path,
        batch_size=args.batch_size,
        base_filters=args.base_filters,
        depth=args.depth,
        seed=args.seed,
        architecture=args.architecture,
        fusion=args.fusion,
    )
