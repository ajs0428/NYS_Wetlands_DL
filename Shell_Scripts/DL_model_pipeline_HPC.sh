#!/bin/bash -l
set -e  # Exit on error

# === CONFIGURATION ===
USE_ASPP=false             # true to enable ASPP at U-Net bottleneck
ASPP_RATES="3 6 12"      # dilation rates for ASPP; use "3 6 12" for depth=5, "6 12 18" for
KFOLD=0                    # 0=disabled, 2+=run k-fold CV instead of single split
BASE_FILTERS=64
DEPTH=4
BATCH_SIZE=32
EPOCHS=100
SEED=420
WORKERS=4

# To switch between binary and multiclass, edit classification_mode in dl_band_config.json
# before running the pipeline — step 1 (dl_01_compute_statistics.py)

# === PATHS (relative to project root) ===
PATCHES_DIR="Data/Training_Data/R_Patches"
STATS_PATH="Data/Training_Data/normalization_stats.json"
BAND_CONFIG="Python_Code_Analysis/DL_Pipeline_v2/dl_band_config.json"
GLOBAL_STATS="Data/Training_Data/HUC_DL_Stacks_Extracted_Values.json"
SCRIPT_DIR="Python_Code_Analysis/DL_Pipeline_v2"

# Build optional flags
ASPP_FLAGS=""
if [ "$USE_ASPP" = true ]; then
    ASPP_FLAGS="--use-aspp --aspp-rates $ASPP_RATES"
fi

# Read classification mode from band config
CLASS_MODE=$(python -c "import json; print(json.load(open('$BAND_CONFIG'))['classification_mode'])" 2>/dev/null || echo "multiclass")

echo "=== NYS Wetlands DL Pipeline (HPC) ==="
echo "Classification: $CLASS_MODE"
echo "Architecture: U-Net (bf=$BASE_FILTERS, depth=$DEPTH)"
[ "$USE_ASPP" = true ] && echo "ASPP: enabled (rates: $ASPP_RATES)"
[ "$KFOLD" -ge 2 ] 2>/dev/null && echo "K-Fold CV: $KFOLD folds"
echo "======================================="

# Step 1: Normalization stats and band configuration
python $SCRIPT_DIR/dl_01_compute_statistics.py \
        --patches-dir $PATCHES_DIR \
        --output $STATS_PATH \
        --config $BAND_CONFIG \
        --global-stats $GLOBAL_STATS

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
        --early-stopping 25 \
        --lr-patience 15 \
        --ce-weight 1.0 \
        --dice-weight 1.5 \
        --focal-gamma 2.0 \
        --label-smoothing 0.0 \
         --lr 5e-5 \
        $ASPP_FLAGS \
        $KFOLD_FLAG

# Skip evaluate/predict steps when running k-fold CV
# (k-fold validates internally across all folds)
if [ "$KFOLD" -ge 2 ] 2>/dev/null; then
    echo "=== K-Fold CV complete — see results in Models/kfold_*/ ==="
    exit 0
fi

# Find the newest checkpoint — prefer safetensors, fall back to .ckpt
BEST_MODEL=$(ls -t Models/best_*.safetensors 2>/dev/null | head -1)
if [ -z "$BEST_MODEL" ]; then
    BEST_MODEL=$(ls -t Models/best_*.ckpt 2>/dev/null | head -1)
fi
if [ -z "$BEST_MODEL" ]; then
    echo "ERROR: No checkpoints found in Models/" >&2
    exit 1
fi
echo "Using checkpoint: $BEST_MODEL"

# Derive output name (strip either .safetensors or .ckpt)
EVAL_OUTPUT="${BEST_MODEL%.*}_evaluation_metrics.json"

# Step 3: Evaluate the model
# Architecture params are auto-detected from checkpoint/sidecar metadata;
# CLI flags here serve as fallback for legacy checkpoints only.
python $SCRIPT_DIR/dl_05_evaluate.py \
        --model "$BEST_MODEL" \
        --output "$EVAL_OUTPUT" \
        --patches-dir $PATCHES_DIR \
        --stats-path $STATS_PATH \
        --batch-size $BATCH_SIZE \
        --base-filters $BASE_FILTERS \
        --depth $DEPTH \
        --seed $SEED \
        $ASPP_FLAGS

# # Step 4: Predict
# python $SCRIPT_DIR/dl_06_predict.py \
#         Data/HUC_DL_Stacks/cluster_11_huc_042900030103_stack.tif \
#         Data/HUC_DL_Predictions/DLpred_cluster_11_huc_042900030103.tif \
#         --model "$BEST_MODEL" \
#         --stats $STATS_PATH \
#         --patch-size 256 \
#         --overlap 128 \
#         --base-filters $BASE_FILTERS \
#         --depth $DEPTH \
#         --probs \
#         $ASPP_FLAGS
