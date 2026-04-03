#!/bin/bash -l
set -e  # Exit on error

# === CONFIGURATION ===
USE_ASPP=true             # true to enable ASPP at U-Net bottleneck
ASPP_RATES="3 6 12"      # dilation rates for ASPP; use "3 6 12" for depth=5, "6 12 18" for depth=4
BASE_FILTERS=64
DEPTH=5
BATCH_SIZE=16
SEED=420

# === PATHS (relative to project root) ===
PATCHES_DIR="Data/Training_Data/R_Patches"
STATS_PATH="Data/Training_Data/normalization_stats.json"
BAND_CONFIG="Python_Code_Analysis/DL_Pipeline_v2/dl_band_config.json"
SCRIPT_DIR="Python_Code_Analysis/DL_Pipeline_v2"

# === MODEL SELECTION ===
# Set MODEL_PATH to override automatic checkpoint discovery.
# For a single model:   MODEL_PATH="Models/best_multiclass_unet-v2.ckpt"
# For a k-fold run dir: MODEL_PATH="Models/kfold_2fold_unet_20260401_1430"
# Leave empty to auto-select the newest checkpoint in Models/
MODEL_PATH=""

# Build optional flags
ASPP_FLAGS=""
if [ "$USE_ASPP" = true ]; then
    ASPP_FLAGS="--use-aspp --aspp-rates $ASPP_RATES"
fi

# Read classification mode from band config
CLASS_MODE=$(python -c "import json; print(json.load(open('$BAND_CONFIG'))['classification_mode'])" 2>/dev/null || echo "multiclass")

echo "=== NYS Wetlands DL Evaluation (HPC) ==="
echo "Classification: $CLASS_MODE"
echo "Architecture: U-Net (bf=$BASE_FILTERS, depth=$DEPTH)"
[ "$USE_ASPP" = true ] && echo "ASPP: enabled (rates: $ASPP_RATES)"
echo "=========================================="

# --- Resolve checkpoint(s) ---
evaluate_checkpoint() {
    local CKPT="$1"
    local EVAL_OUTPUT="${CKPT%.ckpt}_evaluation_metrics.json"

    echo ""
    echo "--- Evaluating: $CKPT ---"
    python $SCRIPT_DIR/dl_05_evaluate.py \
            --model "$CKPT" \
            --output "$EVAL_OUTPUT" \
            --patches-dir $PATCHES_DIR \
            --stats-path $STATS_PATH \
            --batch-size $BATCH_SIZE \
            --base-filters $BASE_FILTERS \
            --depth $DEPTH \
            --seed $SEED \
            $ASPP_FLAGS
    echo "Results saved to: $EVAL_OUTPUT"
}

if [ -n "$MODEL_PATH" ]; then
    # User specified a path
    if [ -d "$MODEL_PATH" ]; then
        # It's a directory — treat as k-fold run, evaluate each fold checkpoint
        echo "K-Fold directory: $MODEL_PATH"
        FOLD_CKPTS=( "$MODEL_PATH"/best_*_fold*.ckpt )
        if [ ${#FOLD_CKPTS[@]} -eq 0 ]; then
            echo "ERROR: No fold checkpoints found in $MODEL_PATH" >&2
            exit 1
        fi
        for CKPT in "${FOLD_CKPTS[@]}"; do
            evaluate_checkpoint "$CKPT"
        done
        echo ""
        echo "=== All ${#FOLD_CKPTS[@]} folds evaluated ==="
    else
        # Single checkpoint file
        evaluate_checkpoint "$MODEL_PATH"
    fi
else
    # Auto-select newest checkpoint
    BEST_CKPT=$(ls -t Models/best_*.ckpt 2>/dev/null | head -1)
    if [ -z "$BEST_CKPT" ]; then
        echo "ERROR: No checkpoints found in Models/" >&2
        exit 1
    fi
    evaluate_checkpoint "$BEST_CKPT"
fi
