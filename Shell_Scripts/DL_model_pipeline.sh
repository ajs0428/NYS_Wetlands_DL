#!/bin/bash -l
set -e  # Exit on error

# # Normalization stats and band configuration
# python Python_Code_Analysis/DL_Pipeline_v2/dl_01_compute_statistics.py \
#         --patches-dir Data/Training_Data/R_Patches \
#         --output Data/Training_Data/normalization_stats.json \
#         --config Python_Code_Analysis/DL_Pipeline_v2/dl_band_config.json

# # Train the model
# python Python_Code_Analysis/DL_Pipeline_v2/dl_04_train_lightning.py \
#         --epochs 50 \
#         --batch-size 16 \
#         --base-filters 64 \
#         --depth 4 \
#         --workers 6 \
#         --seed 420 \
#         --early-stopping 15

# Find the newest checkpoint
BEST_CKPT=$(ls -t Models/best_*.ckpt | head -1)
echo "Using checkpoint: $BEST_CKPT"

# Derive output name: best_multiclass_unet-v2.ckpt -> best_multiclass_unet-v2_evaluation_metrics.json
EVAL_OUTPUT="${BEST_CKPT%.ckpt}_evaluation_metrics.json"

# # Evaluate the model
# python Python_Code_Analysis/DL_Pipeline_v2/dl_05_evaluate.py \
#         --model "$BEST_CKPT" \
#         --output "$EVAL_OUTPUT" \
#         --patches-dir Data/Training_Data/R_Patches \
#         --stats-path Data/Training_Data/normalization_stats.json \
#         --batch-size 16 \
#         --base-filters 64 \
#         --seed 420

python Python_Code_Analysis/DL_Pipeline_v2/dl_06_predict.py \
        Data/HUC_DL_Stacks/cluster_208_huc_041402011301_stack.tif \
        Data/HUC_DL_Predictions/DL_pred_cluster_208_huc_04140211301_multiclass_unetbf64_v2.tif \
        --model "$BEST_CKPT" \
        --patch-size 256 \
        --overlap 128 \
        --base-filters 64 \
        --probs