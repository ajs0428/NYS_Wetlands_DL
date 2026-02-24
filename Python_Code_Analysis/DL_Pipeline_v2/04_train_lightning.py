"""
04_train_lightning.py

PyTorch Lightning training for wetland segmentation.
Replaces the manual training loop in 04_train.py with Lightning's Trainer.

The network architecture is passed as a constructor argument (`net`),
making it trivial to swap between UNet, ResUNet34, or any nn.Module
with signature (B, C, H, W) -> (B, num_classes, H, W).
"""

import json
import numpy as np
import torch
import torch.nn as nn
import lightning as L
from lightning.pytorch.callbacks import (
    ModelCheckpoint,
    EarlyStopping,
    LearningRateMonitor,
)
from lightning.pytorch.loggers import CSVLogger
import pandas as pd
from pathlib import Path
from typing import Optional
import importlib.util
import sys


# ── Import sibling modules ──────────────────────────────────────────
def _import_module(name, path):
    if name in sys.modules:
        return sys.modules[name]
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module

_script_dir = Path(__file__).parent
_dataset = _import_module("dataset", _script_dir / "02_dataset.py")
_model = _import_module("unet_model", _script_dir / "03_unet_model.py")

from losses import HybridLoss

create_dataloaders = _dataset.create_dataloaders
UNet = _model.UNet
get_device = _model.get_device


# ── Data Module ──────────────────────────────────────────────────────

class WetlandDataModule(L.LightningDataModule):
    """Wraps existing create_dataloaders() for use with Lightning Trainer."""

    def __init__(
        self,
        patches_dir: Path,
        stats_path: Path,
        batch_size: int = 16,
        num_workers: int = 4,
        seed: int = 42,
    ):
        super().__init__()
        self.patches_dir = patches_dir
        self.stats_path = stats_path
        self.batch_size = batch_size
        self.num_workers = num_workers
        self.seed = seed
        self._train_loader = None
        self._val_loader = None
        self._test_loader = None
        self.class_weights = None

    def setup(self, stage=None):
        if self._train_loader is not None:
            return  # already set up
        train_loader, val_loader, test_loader, class_weights = create_dataloaders(
            self.patches_dir,
            self.stats_path,
            batch_size=self.batch_size,
            num_workers=self.num_workers,
            seed=self.seed,
        )
        self._train_loader = train_loader
        self._val_loader = val_loader
        self._test_loader = test_loader
        self.class_weights = class_weights

    def train_dataloader(self):
        return self._train_loader

    def val_dataloader(self):
        return self._val_loader

    def test_dataloader(self):
        return self._test_loader


# ── Lightning Module ─────────────────────────────────────────────────

class WetlandSegmentationModule(L.LightningModule):
    """
    LightningModule wrapping any segmentation network.

    The network is passed in as `net` — it can be UNet, ResUNet34,
    or any nn.Module with signature (B, C, H, W) -> (B, num_classes, H, W).
    """

    def __init__(
        self,
        net: nn.Module,
        num_classes: int,
        class_weights: Optional[torch.Tensor] = None,
        ignore_index: int = 255,
        learning_rate: float = 1e-4,
        weight_decay: float = 1e-4,
        lr_patience: int = 5,
        ce_weight: float = 1.0,
        dice_weight: float = 1.0,
    ):
        super().__init__()
        self.net = net
        self.num_classes = num_classes
        self.ignore_index = ignore_index
        self.learning_rate = learning_rate
        self.weight_decay = weight_decay
        self.lr_patience = lr_patience

        # Register class_weights as buffer so Lightning moves it to the right device
        if class_weights is not None:
            self.register_buffer("class_weights_buf", class_weights)
        else:
            self.class_weights_buf = None

        self.criterion = HybridLoss(
            num_classes=num_classes,
            weight=self.class_weights_buf,
            ignore_index=ignore_index,
            ce_weight=ce_weight,
            dice_weight=dice_weight,
        )

        self.save_hyperparameters(ignore=["net", "class_weights"])

    def forward(self, x):
        return self.net(x)

    def _shared_step(self, batch, stage: str):
        X, y = batch
        logits = self(X)
        loss = self.criterion(logits, y)

        # Pixel accuracy on valid pixels
        preds = logits.argmax(dim=1)
        valid = y != self.ignore_index
        if valid.any():
            acc = (preds[valid] == y[valid]).float().mean()
        else:
            acc = torch.tensor(0.0, device=self.device)

        # Mean IoU on valid pixels
        if valid.any():
            iou_sum = 0.0
            valid_classes = 0
            for c in range(self.num_classes):
                pred_c = preds[valid] == c
                true_c = y[valid] == c
                intersection = (pred_c & true_c).sum().float()
                union = (pred_c | true_c).sum().float()
                if union > 0:
                    iou_sum += (intersection / union).item()
                    valid_classes += 1
            iou = torch.tensor(
                iou_sum / valid_classes if valid_classes > 0 else 0.0,
                device=self.device,
            )
        else:
            iou = torch.tensor(0.0, device=self.device)

        self.log(f"{stage}/loss", loss, on_step=False, on_epoch=True, prog_bar=True)
        self.log(f"{stage}/acc", acc, on_step=False, on_epoch=True, prog_bar=True)
        self.log(f"{stage}/iou", iou, on_step=False, on_epoch=True, prog_bar=True)
        return loss

    def training_step(self, batch, batch_idx):
        return self._shared_step(batch, "train")

    def validation_step(self, batch, batch_idx):
        self._shared_step(batch, "val")

    def test_step(self, batch, batch_idx):
        self._shared_step(batch, "test")

    def configure_optimizers(self):
        optimizer = torch.optim.AdamW(
            self.parameters(),
            lr=self.learning_rate,
            weight_decay=self.weight_decay,
        )
        scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(
            optimizer, mode="min", factor=0.5, patience=self.lr_patience
        )
        return {
            "optimizer": optimizer,
            "lr_scheduler": {
                "scheduler": scheduler,
                "monitor": "val/loss",
            },
        }


# ── Training entry point ────────────────────────────────────────────

def train(
    patches_dir: Path,
    stats_path: Path,
    output_dir: Path,
    epochs: int = 50,
    batch_size: int = 16,
    learning_rate: float = 1e-4,
    base_filters: int = 32,
    depth: int = 4,
    num_workers: int = 4,
    seed: int = 42,
    early_stopping_patience: int = 15,
):
    """
    Full training pipeline using PyTorch Lightning.

    Args:
        patches_dir: Directory with training patches
        stats_path: Path to normalization_stats.json
        output_dir: Directory to save checkpoints and logs
        epochs: Maximum training epochs
        batch_size: Batch size
        learning_rate: Initial learning rate
        base_filters: U-Net base filter count
        depth: U-Net depth
        num_workers: DataLoader workers
        seed: Random seed
        early_stopping_patience: Epochs to wait before stopping
    """
    L.seed_everything(seed, workers=True)

    # Read configuration from stats
    with open(stats_path) as f:
        stats = json.load(f)
    in_channels = stats["in_channels"]
    num_classes = len(stats["class_names"])
    class_names = stats["class_names"]
    ignore_index = stats.get("ignore_index", 255)
    mode = stats.get("classification_mode", "multiclass")

    print(f"{'='*60}")
    print("Wetland Classification Training (Lightning)")
    print(f"{'='*60}")
    print(f"Classification mode: {mode}")
    print(f"Input channels: {in_channels}, Classes: {num_classes} ({class_names})")
    print(f"Epochs: {epochs}, Batch size: {batch_size}, LR: {learning_rate}")
    print(f"{'='*60}\n")

    # Data
    dm = WetlandDataModule(
        patches_dir, stats_path,
        batch_size=batch_size,
        num_workers=num_workers,
        seed=seed,
    )
    dm.setup()
    class_weights = dm.class_weights
    print(f"Class weights: {class_weights.numpy()}")

    # Network — swap this line for other architectures
    net = UNet(
        in_channels=in_channels,
        num_classes=num_classes,
        base_filters=base_filters,
        depth=depth,
    )

    # Lightning module
    module = WetlandSegmentationModule(
        net=net,
        num_classes=num_classes,
        class_weights=class_weights,
        ignore_index=ignore_index,
        learning_rate=learning_rate,
    )

    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    callbacks = [
        ModelCheckpoint(
            dirpath=output_dir,
            filename=f"best_{mode}",
            monitor="val/loss",
            save_top_k=1,
            mode="min",
        ),
        EarlyStopping(
            monitor="val/loss",
            patience=early_stopping_patience,
            mode="min",
        ),
        LearningRateMonitor(logging_interval="epoch"),
    ]

    csv_logger = CSVLogger(save_dir=output_dir, name="lightning_logs")

    trainer = L.Trainer(
        max_epochs=epochs,
        callbacks=callbacks,
        logger=csv_logger,
        default_root_dir=output_dir,
        log_every_n_steps=10,
        enable_progress_bar=True,
    )

    trainer.fit(module, datamodule=dm)
    trainer.test(module, datamodule=dm)

    # ── Build history dict from CSV log ──────────────────────────────
    metrics_file = Path(csv_logger.log_dir) / "metrics.csv"
    history = {
        "train_loss": [], "val_loss": [],
        "train_accuracy": [], "val_accuracy": [],
        "train_iou": [], "val_iou": [],
    }

    if metrics_file.exists():
        df = pd.read_csv(metrics_file)
        col_map = {
            "train/loss": "train_loss", "val/loss": "val_loss",
            "train/acc": "train_accuracy", "val/acc": "val_accuracy",
            "train/iou": "train_iou", "val/iou": "val_iou",
        }
        for csv_col, hist_key in col_map.items():
            if csv_col in df.columns:
                values = df[csv_col].dropna().tolist()
                history[hist_key] = values

    # Save JSON (parity with legacy 04_train.py)
    history_path = output_dir / f"training_history_{mode}.json"
    with open(history_path, "w") as f:
        json.dump(history, f, indent=2)
    print(f"Training history saved to {history_path}")

    return history


# ── CLI ──────────────────────────────────────────────────────────────

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="Train wetland classification model (Lightning)"
    )
    parser.add_argument("--patches-dir", type=Path,
                        default=Path("Data/Training_Data/R_Patches"))
    parser.add_argument("--stats-path", type=Path,
                        default=Path("Data/Training_Data/normalization_stats.json"))
    parser.add_argument("--output-dir", type=Path, default=Path("Models"))
    parser.add_argument("--epochs", type=int, default=50)
    parser.add_argument("--batch-size", type=int, default=16)
    parser.add_argument("--lr", type=float, default=1e-4)
    parser.add_argument("--base-filters", type=int, default=32)
    parser.add_argument("--depth", type=int, default=4)
    parser.add_argument("--workers", type=int, default=4)
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument("--early-stopping", type=int, default=15,
                        help="Early stopping patience (epochs)")
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
        early_stopping_patience=args.early_stopping,
    )
