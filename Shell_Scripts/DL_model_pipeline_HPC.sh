#!/bin/bash -l
set -e  # Exit on error

# === CONFIGURATION ===
ARCH="unet3plus"                # unet | unet3plus
USE_ASPP=false             # [unet] true to enable ASPP at the bottleneck
ASPP_RATES="6 12 18"      # [unet] dilation rates for ASPP; use "3 6 12" for depth=5, "6 12 18" for depth=4
CAT_CHANNELS=64            # [unet3plus] unified channels per skip branch
DEEP_SUPERVISION=true     # [unet3plus] true to add a loss head per decoder stage + bottleneck
KFOLD=0                    # 0=disabled, 2+=run k-fold CV instead of single split
BASE_FILTERS=64
DEPTH=4
BATCH_SIZE=32
EPOCHS=100
SEED=420
WORKERS=4
PRECISION="bf16-mixed"      # 32-true | 16-mixed | bf16-mixed  (bf16 preferred on Ada/A100/H100; no GradScaler)
# UNet3+ memory hint: full-scale skips are heavier than the plain U-Net, especially
# at DEPTH=5 / high BASE_FILTERS with DEEP_SUPERVISION=true. If you hit CUDA OOM,
# set PRECISION="16-mixed" (or "bf16-mixed" on A100/H100) and/or lower BATCH_SIZE
# (e.g. 16 or 8). These don't change the default unet path (32-true is its default).

# To switch between binary and multiclass, edit classification_mode in dl_band_config.json
# before running the pipeline — step 1 (dl_01_compute_statistics.py)

# === PATHS (relative to project root) ===
PATCHES_DIR="Data/Training_Data/R_Patches"
BAND_CONFIG="Python_Code_Analysis/DL_Pipeline_v2/dl_band_config.json"
GLOBAL_STATS="Data/Training_Data/HUC_DL_Stacks_Extracted_Values.json"
SCRIPT_DIR="Python_Code_Analysis/DL_Pipeline_v2"

# Class-weight power: dl_01 builds the stats with this power and every downstream
# step reads the matching <mode>_normalization_stats[_wp<p>].json (empty "" = base).
# ONE knob -- STATS_PATH below is derived from it rather than restating "wp0.5",
# so the flag and the filename cannot drift apart.
WEIGHT_POWER="0.5"
WP_FLAG=""
[ -n "$WEIGHT_POWER" ] && WP_FLAG="--weight-power $WEIGHT_POWER"

# Build optional flags
ASPP_FLAGS=""
if [ "$USE_ASPP" = true ]; then
    ASPP_FLAGS="--use-aspp --aspp-rates $ASPP_RATES"
fi

# Build architecture flags (only the train step takes these; eval/predict
# auto-detect the architecture from the checkpoint/sidecar metadata)
ARCH_FLAGS="--arch $ARCH"
if [ "$ARCH" = "unet3plus" ]; then
    ARCH_FLAGS="$ARCH_FLAGS --cat-channels $CAT_CHANNELS"
    [ "$DEEP_SUPERVISION" = true ] && ARCH_FLAGS="$ARCH_FLAGS --deep-supervision"
fi

# Read classification mode from band config (display only -- default_stats_path
# reads the same field, so STATS_PATH stays correct if this lookup ever fails).
CLASS_MODE=$(python -c "import json; print(json.load(open('$BAND_CONFIG'))['classification_mode'])" 2>/dev/null || echo "multiclass")

# Resolve the active mode's stats file: <mode>_normalization_stats[_wp<p>].json.
# Same helper dl_01 uses to derive its own default, so --output below is a
# restatement, not an override -- kept explicit because the downstream steps in
# this script read STATS_PATH too, and one in-script definition beats two.
STATS_PATH=$(python -c "import sys; sys.path.insert(0,'$SCRIPT_DIR'); from dl_band_utils import default_stats_path; print(default_stats_path(weight_power=${WEIGHT_POWER:-None}))")

echo "=== NYS Wetlands DL Pipeline (HPC) ==="
echo "Classification: $CLASS_MODE"
echo "Architecture: $ARCH (bf=$BASE_FILTERS, depth=$DEPTH)"
[ "$ARCH" = "unet" ] && [ "$USE_ASPP" = true ] && echo "ASPP: enabled (rates: $ASPP_RATES)"
[ "$ARCH" = "unet3plus" ] && echo "UNet3+: cat_channels=$CAT_CHANNELS, deep_supervision=$DEEP_SUPERVISION"
[ "$KFOLD" -ge 2 ] 2>/dev/null && echo "K-Fold CV: $KFOLD folds"
echo "======================================="

# Step 1: Normalization stats and band configuration
python $SCRIPT_DIR/dl_01_compute_statistics.py \
        --patches-dir $PATCHES_DIR \
        --output $STATS_PATH \
        --config $BAND_CONFIG \
        --global-stats $GLOBAL_STATS \
        $WP_FLAG

# Build k-fold flag
KFOLD_FLAG=""
if [ "$KFOLD" -ge 2 ] 2>/dev/null; then
    KFOLD_FLAG="--kfold $KFOLD"
fi

# Step 2: Train the model
python $SCRIPT_DIR/dl_04_train_lightning.py \
        --stats-path $STATS_PATH \
        --epochs $EPOCHS \
        --batch-size $BATCH_SIZE \
        --base-filters $BASE_FILTERS \
        --depth $DEPTH \
        --workers $WORKERS \
        --seed $SEED \
        --early-stopping 25 \
        --lr-patience 15 \
        --ce-weight 1.0 \
        --dice-weight 1.0 \
        --focal-gamma 2.0 \
        --label-smoothing 0.0 \
        --lr 1e-4 \
        --dropout 0.2 \
        --weight-decay 1e-4 \
        --precision "$PRECISION" \
        $ARCH_FLAGS \
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
        --stats-path $STATS_PATH \
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
