"""
04_train_lightning.py

PyTorch Lightning training for wetland segmentation.
Replaces the manual training loop in 04_train.py with Lightning's Trainer.

The network architecture is passed as a constructor argument (`net`),
making it trivial to swap between UNet, ResUNet34, or any nn.Module
with signature (B, C, H, W) -> (B, num_classes, H, W).
"""

import json
import torch
import torch.nn as nn
import lightning as L
from datetime import datetime

from lightning.pytorch.callbacks import (
    ModelCheckpoint,
    EarlyStopping,
    LearningRateMonitor,
)
from lightning.pytorch.loggers import CSVLogger, TensorBoardLogger
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
_resunet = _import_module("resunet34", _script_dir / "03b_resunet34.py")

from losses import HybridLoss

create_dataloaders = _dataset.create_dataloaders
UNet = _model.UNet
ResUNet34 = _resunet.ResUNet34


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
        class_names: Optional[list] = None,
        ignore_index: int = 255,
        learning_rate: float = 1e-4,
        weight_decay: float = 1e-4,
        lr_patience: int = 5,
        ce_weight: float = 1.0,
        dice_weight: float = 1.0,
        focal_gamma: float = 2.0,
        label_smoothing: float = 0.0,
    ):
        super().__init__()
        self.net = net
        self.num_classes = num_classes
        self.class_names = class_names or [f"class_{i}" for i in range(num_classes)]
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
            focal_gamma=focal_gamma,
            label_smoothing=label_smoothing,
        )

        # Confusion matrices for epoch-level IoU (on-device, updated via scatter_add_)
        for stage in ("train", "val", "test"):
            self.register_buffer(
                f"_cm_{stage}",
                torch.zeros(num_classes, num_classes, dtype=torch.long),
                persistent=False,
            )

        self.save_hyperparameters(ignore=["net", "class_weights"])

    def forward(self, x):
        return self.net(x)

    def _shared_step(self, batch, stage: str):
        X, y = batch
        logits = self(X)
        loss = self.criterion(logits, y)

        # Metrics — detach from autograd, stay on-device
        with torch.no_grad():
            preds = logits.argmax(dim=1)
            valid = y != self.ignore_index
            if valid.any():
                acc = (preds[valid] == y[valid]).float().mean()
                # Accumulate confusion matrix via scatter_add_ (GPU-native on CUDA & MPS)
                p = preds[valid].view(-1)
                t = y[valid].view(-1)
                indices = t * self.num_classes + p
                cm = getattr(self, f"_cm_{stage}")
                ones = torch.ones_like(indices, dtype=torch.long)
                cm.view(-1).scatter_add_(0, indices, ones)
            else:
                acc = torch.tensor(0.0, device=self.device)

        self.log(f"{stage}/loss", loss, on_step=False, on_epoch=True, prog_bar=True)
        self.log(f"{stage}/acc", acc, on_step=False, on_epoch=True, prog_bar=True)
        return loss

    def _compute_and_log_iou(self, stage: str):
        """Compute mean and per-class IoU from the accumulated confusion matrix and reset."""
        cm = getattr(self, f"_cm_{stage}")
        intersection = cm.diag().float()
        union = (cm.sum(dim=1) + cm.sum(dim=0)).float() - intersection
        valid = union > 0

        # Per-class IoU
        per_class_iou = torch.zeros(self.num_classes, device=cm.device)
        if valid.any():
            per_class_iou[valid] = intersection[valid] / union[valid]
        for i, name in enumerate(self.class_names):
            if valid[i]:
                self.log(f"{stage}/iou_{name}", per_class_iou[i].item(), on_epoch=True)

        # Mean IoU (over classes present in the data)
        mean_iou = per_class_iou[valid].mean().item() if valid.any() else 0.0
        self.log(f"{stage}/iou", mean_iou, prog_bar=True)
        cm.zero_()

    def training_step(self, batch, batch_idx):
        return self._shared_step(batch, "train")

    def validation_step(self, batch, batch_idx):
        self._shared_step(batch, "val")

    def test_step(self, batch, batch_idx):
        self._shared_step(batch, "test")

    def on_train_epoch_end(self):
        self._compute_and_log_iou("train")

    def on_validation_epoch_end(self):
        self._compute_and_log_iou("val")

    def on_test_epoch_end(self):
        self._compute_and_log_iou("test")

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
    weight_decay: float = 1e-4,
    base_filters: int = 32,
    depth: int = 4,
    num_workers: int = 4,
    seed: Optional[int] = None,
    early_stopping_patience: int = 15,
    lr_patience: int = 5,
    architecture: str = "unet",
    precision: str = "32-true",
    ce_weight: float = 1.0,
    dice_weight: float = 1.0,
    focal_gamma: float = 2.0,
    label_smoothing: float = 0.0,
    gradient_clip_val: float = 1.0,
    dropout: float = 0.2,
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
        weight_decay: AdamW weight decay
        base_filters: U-Net base filter count
        depth: U-Net depth
        num_workers: DataLoader workers
        seed: Random seed
        early_stopping_patience: Epochs to wait before early stopping
        lr_patience: Epochs to wait before reducing LR
        architecture: Model architecture ('unet' or 'resunet34')
        precision: Training precision ('32-true', '16-mixed', 'bf16-mixed')
        ce_weight: Cross-entropy weight in hybrid loss
        dice_weight: Dice weight in hybrid loss
        focal_gamma: Focal loss gamma (0 = plain CE)
        label_smoothing: Label smoothing factor
        gradient_clip_val: Max gradient norm for clipping (0 = disabled)
        dropout: Spatial dropout rate after bottleneck (0 = disabled)
    """
    if seed is None:
        seed = int(torch.randint(0, 2**31, (1,)).item())
        print(f"No seed specified — using random seed: {seed}")
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
    print(f"Architecture: {architecture}")
    print(f"Classification mode: {mode}")
    print(f"Input channels: {in_channels}, Classes: {num_classes} ({class_names})")
    print(f"Epochs: {epochs}, Batch size: {batch_size}, LR: {learning_rate}")
    print(f"Precision: {precision}")
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
    print(f"Class weights: {class_weights.tolist()}")

    # Network — select architecture
    if architecture == "resunet34":
        net = ResUNet34(
            in_channels=in_channels,
            num_classes=num_classes,
            base_filters=base_filters,
            dropout=dropout,
        )
    else:
        net = UNet(
            in_channels=in_channels,
            num_classes=num_classes,
            base_filters=base_filters,
            depth=depth,
            dropout=dropout,
        )

    # Lightning module
    module = WetlandSegmentationModule(
        net=net,
        num_classes=num_classes,
        class_weights=class_weights,
        class_names=class_names,
        ignore_index=ignore_index,
        learning_rate=learning_rate,
        weight_decay=weight_decay,
        lr_patience=lr_patience,
        ce_weight=ce_weight,
        dice_weight=dice_weight,
        focal_gamma=focal_gamma,
        label_smoothing=label_smoothing,
    )

    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    callbacks = [
        ModelCheckpoint(
            dirpath=output_dir,
            filename=f"best_{mode}_{architecture}",
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

    timestamp = datetime.now().strftime("%Y%m%d_%H%M")
    run_name = f"{architecture}_bf{base_filters}_{timestamp}"
    csv_logger = CSVLogger(save_dir=output_dir, name="lightning_logs", version=run_name)
    tb_logger = TensorBoardLogger(save_dir=output_dir, name="tb_logs", version=run_name)

    trainer = L.Trainer(
        max_epochs=epochs,
        callbacks=callbacks,
        logger=[csv_logger, tb_logger],
        default_root_dir=output_dir,
        precision=precision,
        gradient_clip_val=gradient_clip_val or None,
        log_every_n_steps=10,
        enable_progress_bar=True,
    )

    trainer.fit(module, datamodule=dm)
    trainer.test(module, datamodule=dm)

    # Report best checkpoint
    best_path = trainer.checkpoint_callback.best_model_path
    best_score = trainer.checkpoint_callback.best_model_score
    print(f"\n{'='*60}")
    print(f"Best model: {best_path}")
    print(f"Best val/loss: {best_score:.4f}")
    print(f"{'='*60}\n")

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
        # Add per-class IoU columns
        for name in class_names:
            for stage in ("train", "val"):
                csv_col = f"{stage}/iou_{name}"
                hist_key = f"{stage}_iou_{name}"
                col_map[csv_col] = hist_key
                history[hist_key] = []

        for csv_col, hist_key in col_map.items():
            if csv_col in df.columns:
                values = df[csv_col].dropna().tolist()
                history[hist_key] = values

    # Save JSON (parity with legacy 04_train.py)
    history_path = output_dir / f"training_history_{mode}_{architecture}.json"
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
    parser.add_argument("--weight-decay", type=float, default=1e-4,
                        help="AdamW weight decay (default: 1e-4)")
    parser.add_argument("--base-filters", type=int, default=32)
    parser.add_argument("--depth", type=int, default=4)
    parser.add_argument("--workers", type=int, default=4)
    parser.add_argument("--seed", type=int, default=None,
                        help="Random seed (default: random each run, set for reproducibility)")
    parser.add_argument("--early-stopping", type=int, default=15,
                        help="Early stopping patience (epochs)")
    parser.add_argument("--lr-patience", type=int, default=5,
                        help="ReduceLROnPlateau patience (epochs, default: 5)")
    parser.add_argument("--architecture", type=str, default="unet",
                        choices=["unet", "resunet34"],
                        help="Model architecture (default: unet)")
    parser.add_argument("--precision", type=str, default="32-true",
                        choices=["32-true", "16-mixed", "bf16-mixed"],
                        help="Training precision (default: 32-true)")
    parser.add_argument("--ce-weight", type=float, default=1.0,
                        help="Cross-entropy weight in hybrid loss (default: 1.0)")
    parser.add_argument("--dice-weight", type=float, default=1.0,
                        help="Dice weight in hybrid loss (default: 1.0)")
    parser.add_argument("--focal-gamma", type=float, default=2.0,
                        help="Focal loss gamma (0=plain CE, default: 2.0)")
    parser.add_argument("--label-smoothing", type=float, default=0.0,
                        help="Label smoothing factor (default: 0.0)")
    parser.add_argument("--gradient-clip-val", type=float, default=1.0,
                        help="Max gradient norm for clipping (0=disabled, default: 1.0)")
    parser.add_argument("--dropout", type=float, default=0.2,
                        help="Spatial dropout after bottleneck (0=disabled, default: 0.2)")
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
        weight_decay=args.weight_decay,
        base_filters=args.base_filters,
        depth=args.depth,
        num_workers=args.workers,
        seed=args.seed,
        early_stopping_patience=args.early_stopping,
        lr_patience=args.lr_patience,
        architecture=args.architecture,
        precision=args.precision,
        ce_weight=args.ce_weight,
        dice_weight=args.dice_weight,
        focal_gamma=args.focal_gamma,
        label_smoothing=args.label_smoothing,
        gradient_clip_val=args.gradient_clip_val,
        dropout=args.dropout,
    )
