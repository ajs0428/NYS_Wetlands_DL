"""
losses.py

Loss functions for wetland segmentation training.
Extracted from 04_train.py for reuse across training scripts.
"""

import torch
import torch.nn as nn


class FocalLoss(nn.Module):
    """Focal Loss for multi-class segmentation.

    Down-weights easy/well-classified pixels so the model focuses on hard
    examples (e.g. minority-class wetland pixels near boundaries).

    FL(p_t) = -alpha_t * (1 - p_t)^gamma * log(p_t)

    When gamma=0 this reduces to standard weighted CrossEntropy.
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
        ce_loss = nn.functional.cross_entropy(
            inputs, targets,
            weight=self.weight,
            ignore_index=self.ignore_index,
            reduction="none",
            label_smoothing=self.label_smoothing,
        )
        # p_t = probability of the correct class
        log_pt = -ce_loss
        pt = torch.exp(log_pt)
        focal = ((1.0 - pt) ** self.gamma) * ce_loss

        # Average over valid pixels
        valid = targets != self.ignore_index
        if valid.any():
            return focal[valid].mean()
        return focal.mean()


class DiceLoss(nn.Module):
    """Per-class Dice loss averaged over classes, with ignore_index support."""

    def __init__(self, num_classes: int, ignore_index: int = 255, smooth: float = 1.0):
        super().__init__()
        self.num_classes = num_classes
        self.ignore_index = ignore_index
        self.smooth = smooth

    def forward(self, inputs: torch.Tensor, targets: torch.Tensor) -> torch.Tensor:
        """
        Args:
            inputs: (B, C, H, W) raw logits
            targets: (B, H, W) integer class labels
        """
        probs = torch.softmax(inputs, dim=1)  # (B, C, H, W)

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
        return 1.0 - dice.mean()


class HybridLoss(nn.Module):
    """Combined Focal + Dice loss.

    Focal Loss (replaces plain CE) down-weights easy examples and carries
    class weights for imbalance handling.
    Dice is inherently class-balanced (per-class then averaged).
    """

    def __init__(
        self,
        num_classes: int,
        weight: torch.Tensor = None,
        ignore_index: int = 255,
        ce_weight: float = 0.5,
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
        focal_loss = self.focal(inputs, targets)
        dice_loss = self.dice(inputs, targets)
        if torch.isnan(focal_loss):
            return self.dice_weight * dice_loss
        return self.ce_weight * focal_loss + self.dice_weight * dice_loss
