"""
dl_losses.py

Loss functions for wetland segmentation training.
Extracted from dl_04_train.py for reuse across training scripts.
"""

import torch
import torch.nn as nn


class FocalLoss(nn.Module):
    """Focal Loss for multi-class segmentation.

    Down-weights easy/well-classified pixels so the model focuses on hard
    examples (e.g. minority-class wetland pixels near boundaries).

    FL(p_t) = -alpha_t * (1 - p_t)^gamma * log(p_t)

    When gamma=0 this reduces to standard weighted CrossEntropy.

    Class weights are applied *after* focal modulation so that pt remains
    a true probability and the focal term works correctly for rare classes.
    """

    def __init__(
        self,
        weight: torch.Tensor = None,
        ignore_index: int = 255,
        gamma: float = 2.0,
        label_smoothing: float = 0.0,
    ):
        super().__init__()
        self.register_buffer("weight", weight)
        self.ignore_index = ignore_index
        self.gamma = gamma
        self.label_smoothing = label_smoothing

    def forward(self, inputs: torch.Tensor, targets: torch.Tensor) -> torch.Tensor:
        """
        Args:
            inputs: (B, C, H, W) raw logits
            targets: (B, H, W) integer class labels
        """
        # Unweighted CE to get true pt for focal modulation
        ce_unweighted = nn.functional.cross_entropy(
            inputs, targets,
            ignore_index=self.ignore_index,
            reduction="none",
            label_smoothing=self.label_smoothing,
        )
        # p_t = true probability of the correct class
        pt = torch.exp(-ce_unweighted)
        focal = ((1.0 - pt) ** self.gamma) * ce_unweighted

        # Apply class weights post-hoc (preserves focal modulation)
        valid = targets != self.ignore_index
        if self.weight is not None and valid.any():
            per_pixel_weight = self.weight[targets.clamp(0, self.weight.shape[0] - 1)]
            focal = focal * per_pixel_weight

        if valid.any():
            return focal[valid].mean()
        return focal.mean()


class DiceLoss(nn.Module):
    """Per-class Dice loss averaged over classes, with ignore_index support."""

    def __init__(self, num_classes: int, ignore_index: int = 255, smooth: float = 0.1):
        super().__init__()
        self.num_classes = num_classes
        self.ignore_index = ignore_index
        self.smooth = smooth

    def forward(self, inputs: torch.Tensor, targets: torch.Tensor,
                probs: torch.Tensor = None) -> torch.Tensor:
        """
        Args:
            inputs: (B, C, H, W) raw logits (unused if probs provided)
            targets: (B, H, W) integer class labels
            probs: (B, C, H, W) pre-computed softmax (optional, avoids recomputation)
        """
        if probs is None:
            probs = torch.softmax(inputs, dim=1)

        # Mask for valid (labeled) pixels
        valid_mask = (targets != self.ignore_index).unsqueeze(1)  # (B, 1, H, W)

        # One-hot encode targets; set ignored pixels to 0 temporarily
        targets_clean = targets.clone()
        targets_clean[targets == self.ignore_index] = 0
        targets_onehot = torch.zeros_like(probs)
        targets_onehot.scatter_(1, targets_clean.unsqueeze(1), 1)

        # Zero out ignored pixels in both tensors
        probs = probs * valid_mask
        targets_onehot = targets_onehot * valid_mask

        # Per-class Dice score (sum over batch, H, W)
        dims = (0, 2, 3)
        intersection = (probs * targets_onehot).sum(dim=dims)
        cardinality = probs.sum(dim=dims) + targets_onehot.sum(dim=dims)

        dice = (2.0 * intersection + self.smooth) / (cardinality + self.smooth)

        # Only average over classes present in the batch (ground-truth pixels > 0).
        # Absent classes get an inflated score from smoothing and bias the mean.
        present = targets_onehot.sum(dim=dims) > 0
        if present.any():
            return 1.0 - dice[present].mean()
        return 1.0 - dice.mean()


class HybridLoss(nn.Module):
    """Combined Focal + Dice loss.

    Softmax is computed once and shared by both sub-losses.
    Focal Loss handles hard-example focus; class weights are applied post-hoc
    so they don't distort the focal probability term.
    Dice is inherently class-balanced (per-class then averaged).
    """

    def __init__(
        self,
        num_classes: int,
        weight: torch.Tensor = None,
        ignore_index: int = 255,
        ce_weight: float = 1.0,
        dice_weight: float = 1.0,
        focal_gamma: float = 2.0,
        label_smoothing: float = 0.0,
    ):
        super().__init__()
        self.focal = FocalLoss(
            weight=weight,
            ignore_index=ignore_index,
            gamma=focal_gamma,
            label_smoothing=label_smoothing,
        )
        self.dice = DiceLoss(num_classes=num_classes, ignore_index=ignore_index)
        self.ce_weight = ce_weight
        self.dice_weight = dice_weight

    def forward(self, inputs: torch.Tensor, targets: torch.Tensor) -> torch.Tensor:
        probs = torch.softmax(inputs, dim=1)
        focal_loss = self.focal(inputs, targets)
        dice_loss = self.dice(inputs, targets, probs=probs)
        if torch.isnan(focal_loss):
            return self.dice_weight * dice_loss
        return self.ce_weight * focal_loss + self.dice_weight * dice_loss
