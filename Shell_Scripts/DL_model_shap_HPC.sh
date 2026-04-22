#!/bin/bash -l
set -e  # Exit on error

# === CONFIGURATION ===
USE_ASPP=true             # true to enable ASPP at U-Net bottleneck (fallback only — auto-detected from checkpoint)
ASPP_RATES="3 6 12"       # fallback ASPP rates (ignored when checkpoint has hparams)
BASE_FILTERS=64           # fallback (ignored when checkpoint has hparams)
DEPTH=5                   # fallback (ignored when checkpoint has hparams)
SEED=420                  # must match training seed to reproduce the test split
N_BACKGROUND=50
N_TEST=20
CROP_SIZE=128             # center-crop to reduce memory; 0 to disable

# === PATHS (relative to project root) ===
PATCHES_DIR="Data/Training_Data/R_Patches"
STATS_PATH="Data/Training_Data/normalization_stats.json"
OUTPUT_DIR="Models/SHAP"
SCRIPT_DIR="Python_Code_Analysis/DL_Pipeline_v2"

# === MODEL SELECTION ===
# Specify one or more checkpoints. Accepts a single file, a directory (k-fold),
# or a space-separated list. Leave empty to auto-select the newest in Models/.
#   MODEL_PATH="Models/best_multiclass_unet-v2.ckpt"
#   MODEL_PATH="Models/model_a.ckpt Models/model_b.safetensors"
#   MODEL_PATH="Models/kfold_2fold_unet_20260401_1430"
MODEL_PATH=""

ASPP_FLAGS=""
if [ "$USE_ASPP" = true ]; then
    ASPP_FLAGS="--use-aspp --aspp-rates $ASPP_RATES"
fi

echo "=== NYS Wetlands DL SHAP Analysis (HPC) ==="
echo "Output dir: $OUTPUT_DIR"
echo "Background patches: $N_BACKGROUND   Test patches: $N_TEST   Crop: $CROP_SIZE"
echo "============================================"

run_shap() {
    local MODEL="$1"
    echo ""
    echo "--- SHAP: $MODEL ---"
    python $SCRIPT_DIR/dl_07_shap_analysis.py \
        --model "$MODEL" \
        --patches-dir $PATCHES_DIR \
        --stats-path $STATS_PATH \
        --output-dir $OUTPUT_DIR \
        --seed $SEED \
        --n-background $N_BACKGROUND \
        --n-test $N_TEST \
        --crop-size $CROP_SIZE \
        --base-filters $BASE_FILTERS \
        --depth $DEPTH \
        $ASPP_FLAGS
}

# Resolve model list
MODELS=()
if [ -n "$MODEL_PATH" ]; then
    for P in $MODEL_PATH; do
        if [ -d "$P" ]; then
            FOLD_MODELS=( "$P"/best_*_fold*.safetensors )
            if [ ${#FOLD_MODELS[@]} -eq 0 ] || [ ! -e "${FOLD_MODELS[0]}" ]; then
                FOLD_MODELS=( "$P"/best_*_fold*.ckpt )
            fi
            if [ ${#FOLD_MODELS[@]} -eq 0 ] || [ ! -e "${FOLD_MODELS[0]}" ]; then
                echo "ERROR: No fold checkpoints found in $P" >&2
                exit 1
            fi
            MODELS+=( "${FOLD_MODELS[@]}" )
        else
            MODELS+=( "$P" )
        fi
    done
else
    BEST_MODEL=$(ls -t Models/best_*.safetensors 2>/dev/null | head -1)
    if [ -z "$BEST_MODEL" ]; then
        BEST_MODEL=$(ls -t Models/best_*.ckpt 2>/dev/null | head -1)
    fi
    if [ -z "$BEST_MODEL" ]; then
        echo "ERROR: No checkpoints found in Models/" >&2
        exit 1
    fi
    MODELS+=( "$BEST_MODEL" )
fi

for MODEL in "${MODELS[@]}"; do
    run_shap "$MODEL"
done

echo ""
echo "=== SHAP complete for ${#MODELS[@]} model(s). Outputs in $OUTPUT_DIR ==="
