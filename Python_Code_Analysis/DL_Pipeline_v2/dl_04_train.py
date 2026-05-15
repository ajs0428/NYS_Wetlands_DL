"""
dl_04_train.py

Training loop for wetland classification U-Net model (legacy fallback).
Supports class weighting, checkpointing, and multiple devices.
"""

import json
import time
import numpy as np
import torch
import torch.nn as nn
from torch.optim import AdamW
from torch.optim.lr_scheduler import ReduceLROnPlateau
from pathlib import Path
from datetime import datetime
from typing import Dict, Optional

from dl_02_dataset import create_dataloaders
from dl_03_unet_model import create_model, get_device
from dl_band_utils import default_stats_path
from dl_losses import HybridLoss


def compute_metrics(
    outputs: torch.Tensor,
    targets: torch.Tensor,
    num_classes: int,
    ignore_index: int
) -> Dict[str, float]:
    """
    Compute pixel accuracy and per-class IoU, excluding unlabeled pixels.

    Args:
        outputs: Model outputs (batch, num_classes, H, W)
        targets: Ground truth labels (batch, H, W)
        num_classes: Number of classes
        ignore_index: Label value to exclude from metrics

    Returns:
        Dictionary with accuracy and IoU metrics
    """
    preds = outputs.argmax(dim=1)  # (batch, H, W)

    # Mask out unlabeled pixels
    valid_mask = targets != ignore_index

    # Pixel accuracy (only labeled pixels)
    valid_preds = preds[valid_mask]
    valid_targets = targets[valid_mask]
    total = valid_targets.numel()
    correct = (valid_preds == valid_targets).sum().item()
    accuracy = correct / total if total > 0 else 0.0

    # Per-class IoU (only labeled pixels)
    ious = []
    for c in range(num_classes):
        pred_c = (valid_preds == c)
        target_c = (valid_targets == c)

        intersection = (pred_c & target_c).sum().item()
        union = (pred_c | target_c).sum().item()

        if union > 0:
            ious.append(intersection / union)

    mean_iou = np.mean(ious) if ious else 0.0

    return {
        "accuracy": accuracy,
        "mean_iou": mean_iou
    }


def train_epoch(
    model: nn.Module,
    loader,
    criterion: nn.Module,
    optimizer,
    device: torch.device,
    epoch: int,
    num_classes: int,
    ignore_index: int
) -> Dict[str, float]:
    """
    Train for one epoch.

    Returns:
        Dictionary with training metrics
    """
    model.train()
    total_loss = 0.0
    total_correct = 0
    total_pixels = 0
    all_ious = []

    for batch_idx, (X, y) in enumerate(loader):
        X = X.to(device)
        y = y.to(device)

        optimizer.zero_grad()
        outputs = model(X)
        loss = criterion(outputs, y)
        loss.backward()
        optimizer.step()

        total_loss += loss.item()

        # Compute batch metrics
        with torch.no_grad():
            metrics = compute_metrics(outputs, y, num_classes, ignore_index)
            total_correct += int(metrics["accuracy"] * y.numel())
            total_pixels += y.numel()
            all_ious.append(metrics["mean_iou"])

        # Progress update every 50 batches
        if (batch_idx + 1) % 50 == 0:
            print(f"  Batch {batch_idx + 1}/{len(loader)} - Loss: {loss.item():.4f}")

    avg_loss = total_loss / len(loader)
    accuracy = total_correct / total_pixels
    mean_iou = np.mean(all_ious)

    return {
        "loss": avg_loss,
        "accuracy": accuracy,
        "mean_iou": mean_iou
    }


@torch.no_grad()
def validate_epoch(
    model: nn.Module,
    loader,
    criterion: nn.Module,
    device: torch.device,
    num_classes: int,
    ignore_index: int
) -> Dict[str, float]:
    """
    Validate for one epoch.

    Returns:
        Dictionary with validation metrics
    """
    model.eval()
    total_loss = 0.0
    total_correct = 0
    total_pixels = 0
    all_ious = []

    for X, y in loader:
        X = X.to(device)
        y = y.to(device)

        outputs = model(X)
        loss = criterion(outputs, y)

        total_loss += loss.item()
        metrics = compute_metrics(outputs, y, num_classes, ignore_index)
        total_correct += int(metrics["accuracy"] * y.numel())
        total_pixels += y.numel()
        all_ious.append(metrics["mean_iou"])

    avg_loss = total_loss / len(loader)
    accuracy = total_correct / total_pixels
    mean_iou = np.mean(all_ious)

    return {
        "loss": avg_loss,
        "accuracy": accuracy,
        "mean_iou": mean_iou
    }


def train(
    patches_dir: Path,
    stats_path: Path,
    output_dir: Path,
    epochs: int = 5,
    batch_size: int = 16,
    learning_rate: float = 1e-4,
    base_filters: int = 32,
    depth: int = 4,
    num_workers: int = 4,
    seed: int = 42,
    device: Optional[torch.device] = None,
    use_aspp: bool = False,
    aspp_rates: tuple = (6, 12, 18),
) -> Dict:
    """
    Full training pipeline.

    Args:
        patches_dir: Directory with training patches
        stats_path: Path to normalization stats JSON
        output_dir: Directory to save models and history
        epochs: Number of training epochs
        batch_size: Batch size
        learning_rate: Initial learning rate
        base_filters: U-Net base filter count
        depth: U-Net depth
        num_workers: DataLoader workers
        seed: Random seed
        device: Training device (auto-detect if None)
        use_aspp: Whether to add ASPP module at U-Net bottleneck
        aspp_rates: Dilation rates for ASPP branches

    Returns:
        Training history dictionary
    """
    if device is None:
        device = get_device()

    output_dir.mkdir(parents=True, exist_ok=True)

    # Read model configuration from stats
    with open(stats_path) as f:
        stats = json.load(f)
    in_channels = stats["in_channels"]
    num_classes = len(stats["class_names"])
    class_names = stats["class_names"]
    ignore_index = stats.get("ignore_index", 255)
    classification_mode = stats.get("classification_mode", "multiclass")

    # File naming: best_model_binary.pth, best_model_multiclass.pth, etc.
    mode_suffix = f"_{classification_mode}"

    print(f"{'='*60}")
    print("Wetland Classification Training")
    print(f"{'='*60}")
    print(f"Device: {device}")
    print(f"Classification mode: {classification_mode}")
    print(f"Patches: {patches_dir}")
    print(f"Output: {output_dir}")
    print(f"Epochs: {epochs}, Batch size: {batch_size}, LR: {learning_rate}")
    print(f"Input channels: {in_channels}, Classes: {num_classes} ({class_names})")
    print(f"{'='*60}\n")

    # Create data loaders
    print("Loading data...")
    train_loader, val_loader, test_loader, class_weights = create_dataloaders(
        patches_dir, stats_path,
        batch_size=batch_size,
        num_workers=num_workers,
        seed=seed,
    )

    # Move class weights to device
    class_weights = class_weights.to(device)
    print(f"\nClass weights: {class_weights.cpu().numpy()}")

    # Create model
    print("\nInitializing model...")
    model = create_model(
        in_channels=in_channels,
        num_classes=num_classes,
        base_filters=base_filters,
        depth=depth,
        device=device,
        use_aspp=use_aspp,
        aspp_rates=aspp_rates,
    )

    # Hybrid CE + Dice loss (CE carries class weights; Dice is inherently balanced)
    # focal_gamma=0.0 reduces FocalLoss to plain CE (legacy behavior)
    criterion = HybridLoss(
        num_classes=num_classes,
        weight=class_weights,
        ignore_index=ignore_index,
        ce_weight=1.0,
        dice_weight=1.0,
        focal_gamma=0.0,
    )

    # Optimizer
    optimizer = AdamW(model.parameters(), lr=learning_rate, weight_decay=1e-4)

    # Learning rate scheduler
    scheduler = ReduceLROnPlateau(
        optimizer, mode='min', factor=0.5, patience=5
    )

    # Training history
    history = {
        "train_loss": [],
        "train_accuracy": [],
        "train_iou": [],
        "val_loss": [],
        "val_accuracy": [],
        "val_iou": [],
        "learning_rate": [],
        "epoch_time": []
    }

    best_val_loss = float('inf')
    start_time = datetime.now()

    print(f"\nStarting training at {start_time.strftime('%Y-%m-%d %H:%M:%S')}\n")

    for epoch in range(epochs):
        epoch_start = time.time()
        current_lr = optimizer.param_groups[0]['lr']

        print(f"Epoch {epoch + 1}/{epochs} (LR: {current_lr:.2e})")
        print("-" * 40)

        # Train
        train_metrics = train_epoch(
            model, train_loader, criterion, optimizer, device, epoch,
            num_classes, ignore_index
        )

        # Validate
        val_metrics = validate_epoch(
            model, val_loader, criterion, device,
            num_classes, ignore_index
        )

        epoch_time = time.time() - epoch_start

        # Update scheduler
        scheduler.step(val_metrics["loss"])

        # Record history
        history["train_loss"].append(train_metrics["loss"])
        history["train_accuracy"].append(train_metrics["accuracy"])
        history["train_iou"].append(train_metrics["mean_iou"])
        history["val_loss"].append(val_metrics["loss"])
        history["val_accuracy"].append(val_metrics["accuracy"])
        history["val_iou"].append(val_metrics["mean_iou"])
        history["learning_rate"].append(current_lr)
        history["epoch_time"].append(epoch_time)

        # Print epoch summary
        print(f"  Train - Loss: {train_metrics['loss']:.4f}, "
              f"Acc: {train_metrics['accuracy']:.4f}, "
              f"mIoU: {train_metrics['mean_iou']:.4f}")
        print(f"  Val   - Loss: {val_metrics['loss']:.4f}, "
              f"Acc: {val_metrics['accuracy']:.4f}, "
              f"mIoU: {val_metrics['mean_iou']:.4f}")
        print(f"  Time: {epoch_time:.1f}s")

        # Save best model
        if val_metrics["loss"] < best_val_loss:
            best_val_loss = val_metrics["loss"]
            torch.save({
                'epoch': epoch,
                'model_state_dict': model.state_dict(),
                'optimizer_state_dict': optimizer.state_dict(),
                'val_loss': val_metrics["loss"],
                'val_accuracy': val_metrics["accuracy"],
                'val_iou': val_metrics["mean_iou"],
                'in_channels': in_channels,
                'num_classes': num_classes
            }, output_dir / f"best_model{mode_suffix}.pth")
            print(f"  *** Saved best model (val_loss: {best_val_loss:.4f}) ***")

        print()

    # Save final model
    torch.save({
        'epoch': epochs,
        'model_state_dict': model.state_dict(),
        'optimizer_state_dict': optimizer.state_dict(),
        'in_channels': in_channels,
        'num_classes': num_classes
    }, output_dir / f"final_model{mode_suffix}.pth")

    # Save training history
    history["config"] = {
        "epochs": epochs,
        "batch_size": batch_size,
        "learning_rate": learning_rate,
        "base_filters": base_filters,
        "depth": depth,
        "seed": seed,
        "in_channels": in_channels,
        "num_classes": num_classes,
        "class_weights": class_weights.cpu().tolist(),
        "class_names": class_names
    }

    with open(output_dir / f"training_history{mode_suffix}.json", 'w') as f:
        json.dump(history, f, indent=2)

    # Training summary
    total_time = datetime.now() - start_time
    print(f"{'='*60}")
    print("Training Complete")
    print(f"{'='*60}")
    print(f"Total time: {total_time}")
    print(f"Best validation loss: {best_val_loss:.4f}")
    print(f"Models saved to: {output_dir}")
    print(f"  best_model{mode_suffix}.pth")
    print(f"  final_model{mode_suffix}.pth")
    print(f"  training_history{mode_suffix}.json")

    return history


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Train wetland classification model")
    parser.add_argument("--patches-dir", type=Path, default=Path("Data/Training_Data/R_Patches"))
    parser.add_argument("--stats-path", type=Path, default=default_stats_path())
    parser.add_argument("--output-dir", type=Path, default=Path("Models"))
    parser.add_argument("--epochs", type=int, default=50)
    parser.add_argument("--batch-size", type=int, default=16)
    parser.add_argument("--lr", type=float, default=1e-4)
    parser.add_argument("--base-filters", type=int, default=32)
    parser.add_argument("--depth", type=int, default=4)
    parser.add_argument("--workers", type=int, default=4)
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument("--use-aspp", action="store_true",
                        help="Add ASPP module at U-Net bottleneck")
    parser.add_argument("--aspp-rates", type=int, nargs="+", default=[6, 12, 18],
                        help="Dilation rates for ASPP branches (default: 6 12 18)")
    args = parser.parse_args()

    # Handle relative paths
    project_root = Path(__file__).parent.parent.parent
    patches_dir = project_root / args.patches_dir if not args.patches_dir.is_absolute() else args.patches_dir
    stats_path = project_root / args.stats_path if not args.stats_path.is_absolute() else args.stats_path
    output_dir = project_root / args.output_dir if not args.output_dir.is_absolute() else args.output_dir

    train(
        patches_dir=patches_dir,
        stats_path=stats_path,
        output_dir=output_dir,
        epochs=args.epochs,
        batch_size=args.batch_size,
        learning_rate=args.lr,
        base_filters=args.base_filters,
        depth=args.depth,
        num_workers=args.workers,
        seed=args.seed,
        use_aspp=args.use_aspp,
        aspp_rates=tuple(args.aspp_rates),
    )
