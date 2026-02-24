"""
losses.py

Loss functions for wetland segmentation training.
Extracted from 04_train.py for reuse across training scripts.
"""

import torch
import torch.nn as nn


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
    """Combined CrossEntropy + Dice loss.

    CE carries class weights for imbalance handling.
    Dice is inherently class-balanced (per-class then averaged).
    """

    def __init__(
        self,
        num_classes: int,
        weight: torch.Tensor = None,
        ignore_index: int = 255,
        ce_weight: float = 1.0,
        dice_weight: float = 1.0
    ):
        super().__init__()
        self.ce = nn.CrossEntropyLoss(weight=weight, ignore_index=ignore_index)
        self.dice = DiceLoss(num_classes=num_classes, ignore_index=ignore_index)
        self.ce_weight = ce_weight
        self.dice_weight = dice_weight

    def forward(self, inputs: torch.Tensor, targets: torch.Tensor) -> torch.Tensor:
        ce_loss = self.ce(inputs, targets)
        dice_loss = self.dice(inputs, targets)
        # CE returns nan when all targets are ignore_index; fall back to Dice only
        if torch.isnan(ce_loss):
            return self.dice_weight * dice_loss
        return self.ce_weight * ce_loss + self.dice_weight * dice_loss
