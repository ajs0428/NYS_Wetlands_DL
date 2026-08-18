#!/usr/bin/env bash
# run_shap_factorial.sh -- Phase 3 SHAP for the wetland factorial experiment.
#
# Runs INSIDE the training container (it backprops through each model, so it
# needs the GPU). Wraps dl_09_shap_factorial.py and handles two container quirks
# that bite the bare `python ...` invocation:
#
#   1. --user gives a UID with no home dir -> $HOME defaults to / (unwritable),
#      so matplotlib/pip fail on /.config. We point HOME + MPLCONFIGDIR at the
#      mounted, writable /app tree.
#   2. `shap` is not baked into older images (it post-dates them in pyproject).
#      If `import shap` fails we `pip install --user` it into /app/.local on the
#      mounted volume, where it persists across the container's --rm.
#
# Any args are passed straight through to dl_09 (e.g. --configs / --seeds).
# v2: dl_09 resolves each cell's pools split (dl_patch_pools), covers ALL configs
# by default (incl. the label-source cells), and takes --mode for the binary tree.
#
# Invoke from the GPU node (one short, paste-safe line; once per mode):
#   docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
#     -v /workdir/$USER/nys_wetlands:/app \
#     nys-wetlands-dl bash Shell_Scripts/run_shap_factorial.sh \
#     --results-dir Models/factorial_results_v3/multiclass
#   ... run_shap_factorial.sh --results-dir Models/factorial_results_v3/binary --mode binary
#
# Subset example:
#   ... bash Shell_Scripts/run_shap_factorial.sh --configs fld_chmret_leafoff --seeds 0
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PIPE="$REPO_ROOT/Python_Code_Analysis/DL_Pipeline_v2"
PYTHON="${PYTHON:-python}"

# Writable HOME on the mounted volume (fixes /.config + gives pip --user a base).
export HOME="${HOME_OVERRIDE:-$REPO_ROOT}"
export MPLCONFIGDIR="${MPLCONFIGDIR:-$REPO_ROOT/tmp/mpl}"
mkdir -p "$MPLCONFIGDIR"

# Ensure shap is importable; install into $HOME/.local (persists past --rm) if not.
if ! "$PYTHON" -c "import shap" >/dev/null 2>&1; then
    echo "[setup] shap not in image -> pip install --user 'shap>=0.46' into $HOME/.local"
    "$PYTHON" -m pip install --no-cache-dir --user "shap>=0.46" || {
        echo "[error] pip install shap failed. If the node has no internet, rebuild"
        echo "        the image (shap is in pyproject.toml) or copy in wheels."
        exit 1
    }
fi

echo "=============================================================="
echo " SHAP (factorial)   HOME=$HOME"
echo " args: ${*:-<defaults: all configs, every seed present>}"
echo "=============================================================="

exec "$PYTHON" "$PIPE/dl_09_shap_factorial.py" "$@"
