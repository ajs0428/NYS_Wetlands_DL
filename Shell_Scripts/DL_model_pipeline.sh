#!/bin/bash -l
set -e  # Exit on error

# === CONFIGURATION ===
ARCHITECTURE="unet"        # "unet", "resunet34", or "dualbranch"
FUSION="gated"             # "gated" or "concat" (only used with dualbranch)
USE_ASPP=true             # true to enable ASPP at U-Net bottleneck (unet only)
ASPP_RATES="6 12 18"      # dilation rates for ASPP; use "3 6 12" for depth=5
KFOLD=0                    # 0=disabled, 2+=run k-fold CV instead of single split
BASE_FILTERS=64
DEPTH=4
BATCH_SIZE=16
EPOCHS=50
SEED=420
WORKERS=6

# To switch between binary and multiclass, you edit classification_mode in dl_band_config.json 
# before running the pipeline — step 1 (dl_01_compute_statistics.py)

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
if [ "$USE_ASPP" = true ]; then
    ARCH_FLAGS="$ARCH_FLAGS --use-aspp --aspp-rates $ASPP_RATES"
fi

# Read classification mode from band config
CLASS_MODE=$(python -c "import json; print(json.load(open('$BAND_CONFIG'))['classification_mode'])" 2>/dev/null || echo "multiclass")

echo "=== NYS Wetlands DL Pipeline ==="
echo "Classification: $CLASS_MODE"
echo "Architecture: $ARCHITECTURE"
[ "$ARCHITECTURE" = "dualbranch" ] && echo "Fusion: $FUSION"
[ "$USE_ASPP" = true ] && echo "ASPP: enabled (rates: $ASPP_RATES)"
[ "$KFOLD" -ge 2 ] 2>/dev/null && echo "K-Fold CV: $KFOLD folds"
echo "================================"

# Step 1: Normalization stats and band configuration
python $SCRIPT_DIR/dl_01_compute_statistics.py \
        --patches-dir $PATCHES_DIR \
        --output $STATS_PATH \
        --config $BAND_CONFIG

# Build k-fold flag
KFOLD_FLAG=""
if [ "$KFOLD" -ge 2 ] 2>/dev/null; then
    KFOLD_FLAG="--kfold $KFOLD"
fi

# Step 2: Train the model
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
        $ARCH_FLAGS \
        $KFOLD_FLAG

# Skip evaluate/predict steps when running k-fold CV
# (k-fold validates internally across all folds)
if [ "$KFOLD" -ge 2 ] 2>/dev/null; then
    echo "=== K-Fold CV complete — see results in Models/kfold_*/ ==="
    exit 0
fi

# Find the newest checkpoint
BEST_CKPT=$(ls -t Models/best_*.ckpt | head -1)
echo "Using checkpoint: $BEST_CKPT"

# Derive output name: best_multiclass_unet-v2.ckpt -> best_multiclass_unet-v2_evaluation_metrics.json
EVAL_OUTPUT="${BEST_CKPT%.ckpt}_evaluation_metrics.json"

# Step 3: Evaluate the model
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
python $SCRIPT_DIR/dl_06_predict.py \
        Data/HUC_DL_Stacks/cluster_11_huc_042900030103_stack.tif \
        Data/HUC_DL_Predictions/DLpred_cluster_11_huc_042900030103.tif \
        --model "$BEST_CKPT" \
        --stats $STATS_PATH \
        --patch-size 256 \
        --overlap 128 \
        --base-filters $BASE_FILTERS \
        --depth $DEPTH \
        --probs \
        $ARCH_FLAGS
