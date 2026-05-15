"""
dl_04_train_lightning.py

PyTorch Lightning training for wetland segmentation.
Replaces the manual training loop in dl_04_train.py with Lightning's Trainer.
Uses a U-Net architecture with residual blocks and SE attention.
"""

import json
import numpy as np
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

from dl_02_dataset import create_dataloaders, create_kfold_splits, create_fold_dataloaders
from dl_03_unet_model import UNet
from dl_band_utils import default_stats_path
from dl_losses import HybridLoss
from dl_model_utils import export_safetensors


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


class WetlandFoldDataModule(L.LightningDataModule):
    """DataModule for a single fold of k-fold cross-validation."""

    def __init__(
        self,
        train_files,
        val_files,
        stats_path: Path,
        batch_size: int = 16,
        num_workers: int = 4,
    ):
        super().__init__()
        self.train_files = train_files
        self.val_files = val_files
        self.stats_path = stats_path
        self.batch_size = batch_size
        self.num_workers = num_workers
        self._train_loader = None
        self._val_loader = None
        self.class_weights = None

    def setup(self, stage=None):
        if self._train_loader is not None:
            return
        self._train_loader, self._val_loader, self.class_weights = (
            create_fold_dataloaders(
                self.train_files, self.val_files, self.stats_path,
                self.batch_size, self.num_workers,
            )
        )

    def train_dataloader(self):
        return self._train_loader

    def val_dataloader(self):
        return self._val_loader


# ── Lightning Module ─────────────────────────────────────────────────

class WetlandSegmentationModule(L.LightningModule):
    """
    LightningModule wrapping the U-Net segmentation network.

    The network is passed in as `net` — any nn.Module with
    signature (B, C, H, W) -> (B, num_classes, H, W).
    """

    def __init__(
        self,
        net: nn.Module,
        num_classes: int,
        # Architecture params — stored in checkpoint for reproducibility
        in_channels: int = 1,
        base_filters: int = 32,
        depth: int = 4,
        dropout: float = 0.0,
        use_aspp: bool = False,
        aspp_rates: tuple = (6, 12, 18),
        # Training params
        class_weights: Optional[torch.Tensor] = None,
        class_names: Optional[list] = None,
        classification_mode: str = "multiclass",
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

        # Example input for TensorBoard graph visualization
        self.example_input_array = torch.randn(1, net.in_channels if hasattr(net, 'in_channels') else 1, 256, 256)

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
        # Snapshot confusion matrix before _compute_and_log_iou zeros it
        self._val_cm_snapshot = self._cm_val.clone().cpu()
        self._compute_and_log_iou("val")

    def on_test_epoch_end(self):
        # Snapshot confusion matrix before _compute_and_log_iou zeros it
        self._test_cm_snapshot = self._cm_test.clone().cpu()
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
    precision: str = "32-true",
    ce_weight: float = 1.0,
    dice_weight: float = 1.0,
    focal_gamma: float = 2.0,
    label_smoothing: float = 0.0,
    gradient_clip_val: float = 1.0,
    dropout: float = 0.2,
    use_aspp: bool = False,
    aspp_rates: tuple = (6, 12, 18),
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
        precision: Training precision ('32-true', '16-mixed', 'bf16-mixed')
        ce_weight: Cross-entropy weight in hybrid loss
        dice_weight: Dice weight in hybrid loss
        focal_gamma: Focal loss gamma (0 = plain CE)
        label_smoothing: Label smoothing factor
        gradient_clip_val: Max gradient norm for clipping (0 = disabled)
        dropout: Spatial dropout rate after bottleneck (0 = disabled)
        use_aspp: Whether to add ASPP module at U-Net bottleneck
        aspp_rates: Dilation rates for ASPP branches
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
    print(f"Architecture: UNet" + (f" + ASPP(rates={aspp_rates})" if use_aspp else ""))
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

    # Network
    net = UNet(
        in_channels=in_channels,
        num_classes=num_classes,
        base_filters=base_filters,
        depth=depth,
        dropout=dropout,
        use_aspp=use_aspp,
        aspp_rates=aspp_rates,
    )

    # Lightning module
    module = WetlandSegmentationModule(
        net=net,
        num_classes=num_classes,
        in_channels=in_channels,
        base_filters=base_filters,
        depth=depth,
        dropout=dropout,
        use_aspp=use_aspp,
        aspp_rates=aspp_rates,
        class_weights=class_weights,
        class_names=class_names,
        classification_mode=mode,
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

    run_timestamp = datetime.now().strftime("%Y%m%d_%H%M")
    ckpt_basename = f"best_{mode}_bf{base_filters}_d{depth}_{run_timestamp}"

    callbacks = [
        ModelCheckpoint(
            dirpath=output_dir,
            filename=ckpt_basename,
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
    run_name = f"unet_bf{base_filters}_{timestamp}"
    csv_logger = CSVLogger(save_dir=output_dir, name="lightning_logs", version=run_name)
    tb_logger = TensorBoardLogger(save_dir=output_dir, name="tb_logs", version=run_name,
                                   log_graph=True)

    trainer = L.Trainer(
        max_epochs=epochs,
        callbacks=callbacks,
        logger=[csv_logger, tb_logger],
        default_root_dir=output_dir,
        precision=precision,
        gradient_clip_val=gradient_clip_val or None,
        log_every_n_steps=5,
        enable_progress_bar=True,
    )

    
    trainer.fit(module, datamodule=dm)
    test_results = trainer.test(module, datamodule=dm, ckpt_path="best")

    # Report best checkpoint
    best_path = trainer.checkpoint_callback.best_model_path
    best_score = trainer.checkpoint_callback.best_model_score
    print(f"\n{'='*60}")
    print(f"Best model: {best_path}")
    print(f"Best val/loss: {best_score:.4f}")
    print(f"{'='*60}\n")

    # Export to safetensors for safe, fast inference loading
    if best_path:
        export_safetensors(Path(best_path))

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

    # Save JSON (parity with legacy dl_04_train.py)
    history_path = output_dir / f"training_history_{mode}_unet.json"
    with open(history_path, "w") as f:
        json.dump(history, f, indent=2)
    print(f"Training history saved to {history_path}")

    # ── Training journal ────────────────────────────────────────────
    # Append a structured entry to training_log.json for run-over-run comparison
    test_metrics_dict = test_results[0] if test_results else {}

    # Build per-class metrics from the confusion matrix snapshot
    cm = getattr(module, "_test_cm_snapshot", None)
    per_class = {}
    macro_f1 = 0.0
    if cm is not None:
        cm_float = cm.float()
        for i, name in enumerate(class_names):
            tp = cm_float[i, i].item()
            support = cm_float[i].sum().item()
            pred_total = cm_float[:, i].sum().item()
            precision = tp / pred_total if pred_total > 0 else 0.0
            recall = tp / support if support > 0 else 0.0
            f1 = (2 * precision * recall / (precision + recall)
                   if (precision + recall) > 0 else 0.0)
            iou = test_metrics_dict.get(f"test/iou_{name}", 0.0)
            per_class[name] = {
                "precision": round(precision, 4),
                "recall": round(recall, 4),
                "f1": round(f1, 4),
                "iou": round(iou, 4),
                "support": int(support),
            }
            macro_f1 += f1
        macro_f1 /= len(class_names)

    # Data split sizes
    data_split = {
        "train": len(dm.train_dataloader().dataset),
        "val": len(dm.val_dataloader().dataset),
        "test": len(dm.test_dataloader().dataset),
    }

    journal_entry = {
        "timestamp": datetime.now().isoformat(timespec="seconds"),
        "checkpoint": Path(best_path).name if best_path else None,
        "kfold": False,
        "best_epoch": module.current_epoch,
        "best_val_loss": round(best_score.item(), 4) if best_score is not None else None,
        "config": {
            "in_channels": in_channels,
            "num_classes": num_classes,
            "class_names": class_names,
            "base_filters": base_filters,
            "depth": depth,
            "use_aspp": use_aspp,
            "aspp_rates": list(aspp_rates),
            "dropout": dropout,
            "learning_rate": learning_rate,
            "weight_decay": weight_decay,
            "ce_weight": ce_weight,
            "dice_weight": dice_weight,
            "focal_gamma": focal_gamma,
            "label_smoothing": label_smoothing,
            "batch_size": batch_size,
            "seed": seed,
            "early_stopping_patience": early_stopping_patience,
            "lr_patience": lr_patience,
            "precision": precision,
            "gradient_clip_val": gradient_clip_val,
        },
        "data_split": data_split,
        "test_metrics": {
            "overall_accuracy": round(test_metrics_dict.get("test/acc", 0.0), 4),
            "mean_iou": round(test_metrics_dict.get("test/iou", 0.0), 4),
            "macro_f1": round(macro_f1, 4),
            "per_class": per_class,
        },
        "confusion_matrix": {
            "labels": class_names,
            "matrix": cm.tolist() if cm is not None else None,
        },
    }

    journal_path = output_dir / "training_log.json"
    if journal_path.exists():
        with open(journal_path) as f:
            journal = json.load(f)
    else:
        journal = []
    journal.append(journal_entry)
    with open(journal_path, "w") as f:
        json.dump(journal, f, indent=2)
    print(f"Training journal updated: {journal_path} ({len(journal)} entries)")

    return history


# ── K-Fold Cross-Validation ────────────────────────────────────────

def train_kfold(
    patches_dir: Path,
    stats_path: Path,
    output_dir: Path,
    n_folds: int = 5,
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
    precision: str = "32-true",
    ce_weight: float = 1.0,
    dice_weight: float = 1.0,
    focal_gamma: float = 2.0,
    label_smoothing: float = 0.0,
    gradient_clip_val: float = 1.0,
    dropout: float = 0.2,
    use_aspp: bool = False,
    aspp_rates: tuple = (6, 12, 18),
):
    """
    K-fold cross-validation training.

    Trains one model per fold, collects validation metrics from each,
    and reports mean ± std across folds.

    Args:
        n_folds: Number of cross-validation folds (default: 5)
        (remaining args identical to train())
    """
    if seed is None:
        seed = int(torch.randint(0, 2**31, (1,)).item())
        print(f"No seed specified — using random seed: {seed}")

    # Read configuration from stats
    with open(stats_path) as f:
        stats = json.load(f)
    in_channels = stats["in_channels"]
    num_classes = len(stats["class_names"])
    class_names = stats["class_names"]
    ignore_index = stats.get("ignore_index", 255)
    mode = stats.get("classification_mode", "multiclass")

    print(f"\n{'='*60}")
    print(f"K-FOLD CROSS-VALIDATION ({n_folds} folds)")
    print(f"{'='*60}")
    print(f"Architecture: UNet" + (f" + ASPP(rates={aspp_rates})" if use_aspp else ""))
    print(f"Classification mode: {mode}")
    print(f"Input channels: {in_channels}, Classes: {num_classes} ({class_names})")
    print(f"Epochs: {epochs}, Batch size: {batch_size}, LR: {learning_rate}")
    print(f"Precision: {precision}, Seed: {seed}")
    print(f"{'='*60}\n")

    # Create fold splits
    folds = create_kfold_splits(patches_dir, n_folds=n_folds, seed=seed)

    # Output directory for this CV run
    timestamp = datetime.now().strftime("%Y%m%d_%H%M")
    kfold_dir = output_dir / f"kfold_{n_folds}fold_unet_{timestamp}"
    kfold_dir.mkdir(parents=True, exist_ok=True)

    fold_results = []

    for fold_idx, (train_files, val_files) in enumerate(folds):
        fold_num = fold_idx + 1
        print(f"\n{'='*60}")
        print(f"FOLD {fold_num}/{n_folds}  "
              f"(train={len(train_files)}, val={len(val_files)})")
        print(f"{'='*60}")

        # Seed each fold deterministically
        L.seed_everything(seed + fold_idx, workers=True)

        # Data module for this fold
        dm = WetlandFoldDataModule(
            train_files, val_files, stats_path,
            batch_size=batch_size,
            num_workers=num_workers,
        )
        dm.setup()

        # Fresh network
        net = UNet(
            in_channels=in_channels,
            num_classes=num_classes,
            base_filters=base_filters,
            depth=depth,
            dropout=dropout,
            use_aspp=use_aspp,
            aspp_rates=aspp_rates,
        )

        # Lightning module
        module = WetlandSegmentationModule(
            net=net,
            num_classes=num_classes,
            in_channels=in_channels,
            base_filters=base_filters,
            depth=depth,
            dropout=dropout,
            use_aspp=use_aspp,
            aspp_rates=aspp_rates,
            class_weights=dm.class_weights,
            class_names=class_names,
            classification_mode=mode,
            ignore_index=ignore_index,
            learning_rate=learning_rate,
            weight_decay=weight_decay,
            lr_patience=lr_patience,
            ce_weight=ce_weight,
            dice_weight=dice_weight,
            focal_gamma=focal_gamma,
            label_smoothing=label_smoothing,
        )

        fold_name = f"fold{fold_num}"
        callbacks = [
            ModelCheckpoint(
                dirpath=kfold_dir,
                filename=f"best_{mode}_unet_{fold_name}",
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

        run_name = f"unet_bf{base_filters}_{fold_name}_{timestamp}"
        csv_logger = CSVLogger(
            save_dir=kfold_dir, name="lightning_logs", version=run_name,
        )
        tb_logger = TensorBoardLogger(
            save_dir=kfold_dir, name="tb_logs", version=run_name,
            log_graph=(fold_idx == 0),
        )

        trainer = L.Trainer(
            max_epochs=epochs,
            callbacks=callbacks,
            logger=[csv_logger, tb_logger],
            default_root_dir=kfold_dir,
            precision=precision,
            gradient_clip_val=gradient_clip_val or None,
            log_every_n_steps=5,
            enable_progress_bar=True,
        )

        trainer.fit(module, datamodule=dm)

        # Validate with best checkpoint to get final fold metrics
        val_results = trainer.validate(module, datamodule=dm, ckpt_path="best")

        # Collect per-fold results
        fold_metrics = val_results[0] if val_results else {}
        fold_metrics["fold"] = fold_num
        best_path = trainer.checkpoint_callback.best_model_path
        best_score = trainer.checkpoint_callback.best_model_score
        fold_metrics["best_checkpoint"] = best_path
        if best_path:
            export_safetensors(Path(best_path))
        fold_metrics["best_val_loss"] = (
            best_score.item() if best_score is not None else None
        )

        # Capture confusion matrix for this fold
        fold_cm = getattr(module, "_val_cm_snapshot", None)
        fold_metrics["confusion_matrix"] = fold_cm.tolist() if fold_cm is not None else None
        fold_results.append(fold_metrics)

        val_loss = fold_metrics.get("val/loss", float("nan"))
        val_iou = fold_metrics.get("val/iou", float("nan"))
        print(f"\nFold {fold_num} complete — "
              f"val/loss: {val_loss:.4f}, val/iou: {val_iou:.4f}")

    # ── Aggregate results across folds ──────────────────────────────
    metric_keys = [
        k for k in fold_results[0]
        if isinstance(fold_results[0].get(k), (int, float))
        and k != "fold"
    ]

    summary = {}
    for key in metric_keys:
        values = [
            r[key] for r in fold_results
            if key in r and r[key] is not None
        ]
        if values:
            summary[key] = {
                "mean": round(float(np.mean(values)), 4),
                "std": round(float(np.std(values)), 4),
                "per_fold": [round(v, 4) for v in values],
            }

    # Print summary
    print(f"\n{'='*60}")
    print(f"K-FOLD CROSS-VALIDATION SUMMARY ({n_folds} folds)")
    print(f"{'='*60}")
    for key in sorted(summary.keys()):
        s = summary[key]
        print(f"  {key:20s}: {s['mean']:.4f} ± {s['std']:.4f}  {s['per_fold']}")
    print(f"{'='*60}\n")

    # Save results JSON
    results = {
        "n_folds": n_folds,
        "architecture": "unet",
        "base_filters": base_filters,
        "depth": depth,
        "seed": seed,
        "epochs": epochs,
        "batch_size": batch_size,
        "learning_rate": learning_rate,
        "weight_decay": weight_decay,
        "ce_weight": ce_weight,
        "dice_weight": dice_weight,
        "focal_gamma": focal_gamma,
        "label_smoothing": label_smoothing,
        "dropout": dropout,
        "use_aspp": use_aspp,
        "per_fold": fold_results,
        "summary": summary,
    }

    results_path = kfold_dir / f"kfold_results_{mode}_unet.json"
    with open(results_path, "w") as f:
        json.dump(results, f, indent=2)
    print(f"Results saved to {results_path}")

    # ── Training journal ────────────────────────────────────────────
    # Build per-class metrics from the summed confusion matrix across folds
    summed_cm = None
    for fr in fold_results:
        cm = fr.get("confusion_matrix")
        if cm is not None:
            cm_tensor = torch.tensor(cm, dtype=torch.float)
            summed_cm = cm_tensor if summed_cm is None else summed_cm + cm_tensor

    per_class = {}
    macro_f1 = 0.0
    if summed_cm is not None:
        for i, name in enumerate(class_names):
            tp = summed_cm[i, i].item()
            support = summed_cm[i].sum().item()
            pred_total = summed_cm[:, i].sum().item()
            precision_val = tp / pred_total if pred_total > 0 else 0.0
            recall_val = tp / support if support > 0 else 0.0
            f1 = (2 * precision_val * recall_val / (precision_val + recall_val)
                   if (precision_val + recall_val) > 0 else 0.0)
            iou_val = summary.get(f"val/iou_{name}", {})
            per_class[name] = {
                "precision": round(precision_val, 4),
                "recall": round(recall_val, 4),
                "f1": round(f1, 4),
                "iou_mean": iou_val.get("mean", 0.0) if isinstance(iou_val, dict) else 0.0,
                "iou_std": iou_val.get("std", 0.0) if isinstance(iou_val, dict) else 0.0,
                "support": int(support),
            }
            macro_f1 += f1
        macro_f1 /= len(class_names)

    # Per-fold journal details (epoch, val_loss, per-fold confusion matrices)
    per_fold_details = []
    for fr in fold_results:
        per_fold_details.append({
            "fold": fr["fold"],
            "best_val_loss": round(fr["best_val_loss"], 4) if fr.get("best_val_loss") is not None else None,
            "val_accuracy": round(fr.get("val/acc", 0.0), 4),
            "val_iou": round(fr.get("val/iou", 0.0), 4),
            "confusion_matrix": fr.get("confusion_matrix"),
        })

    val_loss_summary = summary.get("val/loss", {})
    val_iou_summary = summary.get("val/iou", {})
    val_acc_summary = summary.get("val/acc", {})

    journal_entry = {
        "timestamp": datetime.now().isoformat(timespec="seconds"),
        "kfold": True,
        "n_folds": n_folds,
        "results_dir": str(kfold_dir),
        "config": {
            "in_channels": in_channels,
            "num_classes": num_classes,
            "class_names": class_names,
            "base_filters": base_filters,
            "depth": depth,
            "use_aspp": use_aspp,
            "aspp_rates": list(aspp_rates),
            "dropout": dropout,
            "learning_rate": learning_rate,
            "weight_decay": weight_decay,
            "ce_weight": ce_weight,
            "dice_weight": dice_weight,
            "focal_gamma": focal_gamma,
            "label_smoothing": label_smoothing,
            "batch_size": batch_size,
            "seed": seed,
            "early_stopping_patience": early_stopping_patience,
            "lr_patience": lr_patience,
            "precision": precision,
            "gradient_clip_val": gradient_clip_val,
        },
        "summary_metrics": {
            "val_loss": val_loss_summary if val_loss_summary else None,
            "val_accuracy": val_acc_summary if val_acc_summary else None,
            "val_iou": val_iou_summary if val_iou_summary else None,
            "macro_f1": round(macro_f1, 4),
            "per_class": per_class,
        },
        "confusion_matrix_summed": {
            "labels": class_names,
            "matrix": summed_cm.long().tolist() if summed_cm is not None else None,
        },
        "per_fold": per_fold_details,
    }

    journal_path = output_dir / "training_log.json"
    if journal_path.exists():
        with open(journal_path) as f:
            journal = json.load(f)
    else:
        journal = []
    journal.append(journal_entry)
    with open(journal_path, "w") as f:
        json.dump(journal, f, indent=2)
    print(f"Training journal updated: {journal_path} ({len(journal)} entries)")

    return results


# ── CLI ──────────────────────────────────────────────────────────────

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="Train wetland classification model (Lightning)"
    )
    parser.add_argument("--patches-dir", type=Path,
                        default=Path("Data/Training_Data/R_Patches"))
    parser.add_argument("--stats-path", type=Path,
                        default=default_stats_path())
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
    parser.add_argument("--use-aspp", action="store_true",
                        help="Add ASPP module at U-Net bottleneck for expanded receptive field")
    parser.add_argument("--aspp-rates", type=int, nargs="+", default=[6, 12, 18],
                        help="Dilation rates for ASPP branches (default: 6 12 18)")
    parser.add_argument("--kfold", type=int, default=0,
                        help="Number of cross-validation folds (0=disabled, default: 0). "
                             "When set (e.g. --kfold 5), runs k-fold CV instead of a single train/val/test split.")
    args = parser.parse_args()

    # Handle relative paths
    project_root = Path(__file__).parent.parent.parent
    patches_dir = project_root / args.patches_dir if not args.patches_dir.is_absolute() else args.patches_dir
    stats_path = project_root / args.stats_path if not args.stats_path.is_absolute() else args.stats_path
    output_dir = project_root / args.output_dir if not args.output_dir.is_absolute() else args.output_dir

    # Shared keyword arguments
    shared_kwargs = dict(
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
        precision=args.precision,
        ce_weight=args.ce_weight,
        dice_weight=args.dice_weight,
        focal_gamma=args.focal_gamma,
        label_smoothing=args.label_smoothing,
        gradient_clip_val=args.gradient_clip_val,
        dropout=args.dropout,
        use_aspp=args.use_aspp,
        aspp_rates=tuple(args.aspp_rates),
    )

    if args.kfold >= 2:
        train_kfold(n_folds=args.kfold, **shared_kwargs)
    else:
        train(**shared_kwargs)
