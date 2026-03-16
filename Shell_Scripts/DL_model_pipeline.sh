#!/bin/bash -l
set -e  # Exit on error

# === CONFIGURATION ===
ARCHITECTURE="unet"        # "unet", "resunet34", or "dualbranch"
FUSION="gated"             # "gated" or "concat" (only used with dualbranch)
BASE_FILTERS=64
DEPTH=4
BATCH_SIZE=6
EPOCHS=50
SEED=420
WORKERS=6

# === PATHS (relative to project root) ===
PATCHES_DIR="Data/Training_Data/R_Patches"
STATS_PATH="Data/Training_Data/normalization_stats.json"
BAND_CONFIG="Python_Code_Analysis/DL_Pipeline_v2/dl_band_config.json"
SCRIPT_DIR="Python_Code_Analysis/DL_Pipeline_v2"

# Build architecture flags
ARCH_FLAGS="--architecture $ARCHITECTURE"
if [ "$ARCHITECTURE" = "dualbranch" ]; then
    ARCH_FLAGS="$ARCH_FLAGS --fusion $FUSION"
fi

echo "=== NYS Wetlands DL Pipeline ==="
echo "Architecture: $ARCHITECTURE"
[ "$ARCHITECTURE" = "dualbranch" ] && echo "Fusion: $FUSION"
echo "================================"

# Step 1: Normalization stats and band configuration
python $SCRIPT_DIR/dl_01_compute_statistics.py \
        --patches-dir $PATCHES_DIR \
        --output $STATS_PATH \
        --config $BAND_CONFIG

# Step 2: Train the model
    # script 4
python $SCRIPT_DIR/dl_04_train_lightning.py \
        --epochs $EPOCHS \
        --batch-size $BATCH_SIZE \
        --base-filters $BASE_FILTERS \
        --depth $DEPTH \
        --workers $WORKERS \
        --seed $SEED \
        --early-stopping 15 \
        --lr-patience 10 \
        --dice-weight 1.5 \
        --focal-gamma 2.0 \
        $ARCH_FLAGS

# Find the newest checkpoint
BEST_CKPT=$(ls -t Models/best_*.ckpt | head -1)
echo "Using checkpoint: $BEST_CKPT"

# Derive output name: best_multiclass_unet-v2.ckpt -> best_multiclass_unet-v2_evaluation_metrics.json
EVAL_OUTPUT="${BEST_CKPT%.ckpt}_evaluation_metrics.json"

# Step 3: Evaluate the model
    # script 5
python $SCRIPT_DIR/dl_05_evaluate.py \
        --model "$BEST_CKPT" \
        --output "$EVAL_OUTPUT" \
        --patches-dir $PATCHES_DIR \
        --stats-path $STATS_PATH \
        --batch-size $BATCH_SIZE \
        --base-filters $BASE_FILTERS \
        --depth $DEPTH \
        --seed $SEED \
        $ARCH_FLAGS

# Step 4: Predict
    # script 6
# python $SCRIPT_DIR/dl_06_predict.py \
#         Data/HUC_DL_Stacks/cluster_208_huc_041402011301_stack.tif \
#         Data/HUC_DL_Predictions/DL_pred_cluster_208_huc_04140211301_multiclass_unetbf64_v2.tif \
#         --model "$BEST_CKPT" \
#         --patch-size 256 \
#         --overlap 128 \
#         --base-filters $BASE_FILTERS \
#         --probs \
#         $ARCH_FLAGS
