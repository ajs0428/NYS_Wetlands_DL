from pathlib import Path
import os
workdir = Path("/Users/Anthony/Data and Analysis Local/NYS_Wetlands_GHG/")
print(workdir)
os.chdir(workdir)
current_working_dir = Path.cwd()
print(f"Current working directory is now: {current_working_dir}")

import torch
import torch.nn as nn

class ConvBlock(nn.Module):
    """Two consecutive conv layers with BatchNorm and ReLU."""
    
    def __init__(self, in_channels, out_channels):
        super().__init__()
        self.conv = nn.Sequential(
            nn.Conv2d(in_channels, out_channels, kernel_size=3, padding=1),
            nn.BatchNorm2d(out_channels),
            nn.ReLU(inplace=True),
            nn.Conv2d(out_channels, out_channels, kernel_size=3, padding=1),
            nn.BatchNorm2d(out_channels),
            nn.ReLU(inplace=True)
        )
    
    def forward(self, x):
        return self.conv(x)


class EncoderBlock(nn.Module):
    """ConvBlock followed by MaxPool for downsampling."""
    
    def __init__(self, in_channels, out_channels):
        super().__init__()
        self.conv = ConvBlock(in_channels, out_channels)
        self.pool = nn.MaxPool2d(kernel_size=2, stride=2)
    
    def forward(self, x):
        conv_out = self.conv(x)
        pooled = self.pool(conv_out)
        return conv_out, pooled  # Return both for skip connection


class DecoderBlock(nn.Module):
    """Upsample, concatenate skip connection, then ConvBlock."""
    
    def __init__(self, in_channels, out_channels):
        super().__init__()
        self.upsample = nn.ConvTranspose2d(
            in_channels, out_channels, kernel_size=2, stride=2
        )
        self.conv = ConvBlock(out_channels * 2, out_channels)  # *2 for concatenation
    
    def forward(self, x, skip):
        x = self.upsample(x)
        x = torch.cat([x, skip], dim=1)  # Concatenate along channel dimension
        return self.conv(x)


class UNet(nn.Module):
    """Lightweight U-Net for semantic segmentation."""
    
    def __init__(self, in_channels=7, num_classes=5, base_filters=32):
        """
        Args:
            in_channels: Number of input bands (7: R,G,B,NIR,NDWI,NDVI,DEM)
            num_classes: Number of output classes (5: background + 4 wetland types)
            base_filters: Number of filters in first layer (doubles each level)
        """
        super().__init__()
        
        f = base_filters  # 32
        
        # Encoder path
        self.enc1 = EncoderBlock(in_channels, f)      # 7 -> 32
        self.enc2 = EncoderBlock(f, f * 2)            # 32 -> 64
        self.enc3 = EncoderBlock(f * 2, f * 4)        # 64 -> 128
        self.enc4 = EncoderBlock(f * 4, f * 8)        # 128 -> 256
        
        # Bottleneck
        self.bottleneck = ConvBlock(f * 8, f * 16)    # 256 -> 512
        
        # Decoder path
        self.dec4 = DecoderBlock(f * 16, f * 8)       # 512 -> 256
        self.dec3 = DecoderBlock(f * 8, f * 4)        # 256 -> 128
        self.dec2 = DecoderBlock(f * 4, f * 2)        # 128 -> 64
        self.dec1 = DecoderBlock(f * 2, f)            # 64 -> 32
        
        # Final classification layer
        self.final = nn.Conv2d(f, num_classes, kernel_size=1)
    
    def forward(self, x):
        # Encoder
        skip1, x = self.enc1(x)   # skip1: 32 channels
        skip2, x = self.enc2(x)   # skip2: 64 channels
        skip3, x = self.enc3(x)   # skip3: 128 channels
        skip4, x = self.enc4(x)   # skip4: 256 channels
        
        # Bottleneck
        x = self.bottleneck(x)    # 512 channels
        
        # Decoder with skip connections
        x = self.dec4(x, skip4)   # 256 channels
        x = self.dec3(x, skip3)   # 128 channels
        x = self.dec2(x, skip2)   # 64 channels
        x = self.dec1(x, skip1)   # 32 channels
        
        # Output
        return self.final(x)      # num_classes channels

# === TEST THE MODEL ===
if __name__ == "__main__":
    # Create model
    model = UNet(in_channels=7, num_classes=5, base_filters=32)
    
    # Count parameters
    total_params = sum(p.numel() for p in model.parameters())
    trainable_params = sum(p.numel() for p in model.parameters() if p.requires_grad)
    print(f"Total parameters: {total_params:,}")
    print(f"Trainable parameters: {trainable_params:,}")
    
    # Test forward pass
    dummy_input = torch.randn(4, 7, 128, 128)  # Batch of 4, 7 channels, 128x128
    print(f"\nInput shape: {dummy_input.shape}")
    
    output = model(dummy_input)
    print(f"Output shape: {output.shape}")  # Should be (4, 5, 128, 128)
    
    # Verify output is correct shape for our task
    assert output.shape == (4, 5, 128, 128), "Output shape mismatch!"
    print("\nModel architecture verified successfully!")

#### Check class distribution
import numpy as np

y_train = np.load("Data/Patches/y_train.npy")

# Count pixels per class
classes, counts = np.unique(y_train, return_counts=True)
total = counts.sum()

print("Class distribution:")
for c, count in zip(classes, counts):
    print(f"  Class {c}: {count:,} pixels ({count/total*100:.2f}%)")

# Compute inverse frequency weights
# Higher weight for rarer classes
frequencies = counts / total
weights = 1.0 / frequencies
weights = weights / weights.min()  # Normalize so smallest weight is 1.0

print("\nClass weights (inverse frequency):")
class_names = ['Background', 'EMW', 'FSW', 'SSW', 'OWW']
for c, w in zip(classes, weights):
    print(f"  {class_names[c]}: {w:.2f}")