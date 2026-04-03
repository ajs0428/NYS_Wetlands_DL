#!/bin/bash -l
set -e  # Exit on error

# === CONFIGURATION ===
USE_ASPP=true             # true to enable ASPP at U-Net bottleneck
ASPP_RATES="3 6 12"      # dilation rates for ASPP; use "3 6 12" for depth=5, "6 12 18" for depth=4
BASE_FILTERS=64
DEPTH=5
PATCH_SIZE=256
OVERLAP=128

# === PATHS (relative to project root) ===
STATS_PATH="Data/Training_Data/normalization_stats.json"
BAND_CONFIG="Python_Code_Analysis/DL_Pipeline_v2/dl_band_config.json"
SCRIPT_DIR="Python_Code_Analysis/DL_Pipeline_v2"
INPUT_DIR="Data/HUC_Raster_Stacks/HUC_DL_Stacks"
OUTPUT_DIR="Data/HUC_DL_Predictions"

# === MODEL SELECTION ===
# Set MODEL_PATH to use a specific checkpoint. Leave empty to auto-select newest.
MODEL_PATH=""

# Build optional flags
ASPP_FLAGS=""
if [ "$USE_ASPP" = true ]; then
    ASPP_FLAGS="--use-aspp --aspp-rates $ASPP_RATES"
fi

# Read classification mode from band config
CLASS_MODE=$(python -c "import json; print(json.load(open('$BAND_CONFIG'))['classification_mode'])" 2>/dev/null || echo "multiclass")

echo "=== NYS Wetlands DL Prediction (HPC) ==="
echo "Classification: $CLASS_MODE"
echo "Architecture: U-Net (bf=$BASE_FILTERS, depth=$DEPTH)"
[ "$USE_ASPP" = true ] && echo "ASPP: enabled (rates: $ASPP_RATES)"
echo "Patch: ${PATCH_SIZE}px, Overlap: ${OVERLAP}px"
echo "=========================================="

# Resolve checkpoint
if [ -n "$MODEL_PATH" ]; then
    BEST_CKPT="$MODEL_PATH"
else
    BEST_CKPT=$(ls -t Models/best_*.ckpt 2>/dev/null | head -1)
fi
if [ -z "$BEST_CKPT" ] || [ ! -f "$BEST_CKPT" ]; then
    echo "ERROR: No checkpoint found. Set MODEL_PATH or ensure Models/best_*.ckpt exists." >&2
    exit 1
fi
echo "Using checkpoint: $BEST_CKPT"

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Loop through all raster stacks
RASTER_COUNT=0
for INPUT_RASTER in "$INPUT_DIR"/*.tif; do
    [ -f "$INPUT_RASTER" ] || continue

    # Derive output name: cluster_11_huc_042900030103_stack.tif -> DLpred_cluster_11_huc_042900030103_stack.tif
    BASENAME=$(basename "$INPUT_RASTER")
    OUTPUT_RASTER="$OUTPUT_DIR/DLpred_${BASENAME}"

    # Skip if prediction already exists
    if [ -f "$OUTPUT_RASTER" ]; then
        echo "SKIP (exists): $OUTPUT_RASTER"
        continue
    fi

    echo ""
    echo "--- Predicting: $BASENAME ---"
    python $SCRIPT_DIR/dl_06_predict.py \
            "$INPUT_RASTER" \
            "$OUTPUT_RASTER" \
            --model "$BEST_CKPT" \
            --stats $STATS_PATH \
            --patch-size $PATCH_SIZE \
            --overlap $OVERLAP \
            --base-filters $BASE_FILTERS \
            --depth $DEPTH \
            --probs \
            $ASPP_FLAGS

    RASTER_COUNT=$((RASTER_COUNT + 1))
done

echo ""
echo "=== Prediction complete: $RASTER_COUNT rasters processed ==="
