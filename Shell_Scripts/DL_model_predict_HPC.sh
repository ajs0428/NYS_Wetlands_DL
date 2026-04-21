#!/bin/bash -l
set -e  # Exit on error

# === CONFIGURATION ===
# Architecture params (base_filters, depth, use_aspp, aspp_rates, in_channels,
# num_classes, dropout) are auto-detected from the checkpoint:
#   - .safetensors:  read from sibling .meta.json
#   - .ckpt (Lightning): read from 'hyper_parameters' in the file
# Only prediction-time knobs live here.
PATCH_SIZE=256
OVERLAP=128

# === PATHS (relative to project root) ===
STATS_PATH="Data/Training_Data/normalization_stats.json"
BAND_CONFIG="Python_Code_Analysis/DL_Pipeline_v2/dl_band_config.json"
SCRIPT_DIR="Python_Code_Analysis/DL_Pipeline_v2"
INPUT_DIR="Data/HUC_DL_Stacks/HUC_DL_Stacks/"
OUTPUT_DIR="Data/HUC_DL_Predictions"
TRAINING_LOG="Models/training_log.json"

# === MODEL SELECTION ===
# Set MODEL_PATH to use a specific checkpoint. Leave empty to auto-select newest
# (preferring .safetensors over .ckpt).
MODEL_PATH="Models/best_binary_bf64_d4_20260421_1730.safetensors"

# Read classification mode from band config (purely for the banner)
CLASS_MODE=$(python -c "import json; print(json.load(open('$BAND_CONFIG'))['classification_mode'])" 2>/dev/null || echo "multiclass")

# Resolve checkpoint
if [ -n "$MODEL_PATH" ]; then
    BEST_CKPT="$MODEL_PATH"
else
    BEST_CKPT=$(ls -t Models/best_*.safetensors 2>/dev/null | head -1)
    if [ -z "$BEST_CKPT" ]; then
        BEST_CKPT=$(ls -t Models/best_*.ckpt 2>/dev/null | head -1)
    fi
fi
if [ -z "$BEST_CKPT" ] || [ ! -f "$BEST_CKPT" ]; then
    echo "ERROR: No checkpoint found. Set MODEL_PATH or ensure Models/best_*.{safetensors,ckpt} exists." >&2
    exit 1
fi

# Resolve + echo the architecture that dl_06_predict.py will auto-detect.
# Reads meta.json sidecar (safetensors), hyper_parameters (.ckpt), or the
# matching entry in training_log.json (by checkpoint basename) as a last resort.
ARCH_SUMMARY=$(python - "$BEST_CKPT" "$TRAINING_LOG" <<'PY' 2>/dev/null || true
import json, sys
from pathlib import Path

ckpt = Path(sys.argv[1])
log_path = Path(sys.argv[2])
meta = None

# 1) safetensors sidecar
sidecar = ckpt.with_suffix(".meta.json")
if sidecar.exists():
    meta = json.load(open(sidecar))
# 2) .ckpt hyper_parameters
elif ckpt.suffix == ".ckpt":
    try:
        import torch
        hp = torch.load(ckpt, map_location="cpu", weights_only=False).get("hyper_parameters", {})
        if hp:
            meta = {k: hp.get(k) for k in
                    ("in_channels", "num_classes", "base_filters", "depth",
                     "dropout", "use_aspp", "aspp_rates")}
    except Exception:
        pass
# 3) training_log.json lookup by checkpoint basename
if meta is None and log_path.exists():
    entries = json.load(open(log_path))
    match = next((e for e in entries if e.get("checkpoint") == ckpt.name), None)
    if match:
        meta = match.get("config", {})

if meta:
    bf = meta.get("base_filters", "?")
    dp = meta.get("depth", "?")
    ic = meta.get("in_channels", "?")
    nc = meta.get("num_classes", "?")
    aspp = meta.get("use_aspp", False)
    rates = meta.get("aspp_rates", [])
    line = f"UNet(in={ic}, classes={nc}, bf={bf}, depth={dp}"
    if aspp:
        line += f", aspp={list(rates)}"
    line += ")"
    print(line)
else:
    print("unknown (auto-detection will run at load time)")
PY
)

echo "=== NYS Wetlands DL Prediction (HPC) ==="
echo "Classification: $CLASS_MODE"
echo "Checkpoint:     $BEST_CKPT"
echo "Architecture:   $ARCH_SUMMARY"
echo "Patch:          ${PATCH_SIZE}px, Overlap: ${OVERLAP}px"
echo "=========================================="

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Loop through all raster stacks
RASTER_COUNT=0
for INPUT_RASTER in "$INPUT_DIR"/*.tif; do
    [ -f "$INPUT_RASTER" ] || continue

    # Derive output name: cluster_11_huc_042900030103_stack.tif -> DLpred_cluster_11_huc_042900030103_stack.tif
    BASENAME=$(basename "$INPUT_RASTER")
    OUTPUT_RASTER="$OUTPUT_DIR/DLpred_${CLASS_MODE}_${BASENAME}"

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
            --probs

    RASTER_COUNT=$((RASTER_COUNT + 1))
done

echo ""
echo "=== Prediction complete: $RASTER_COUNT rasters processed ==="
