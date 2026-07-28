#!/usr/bin/env bash
# run_tensorboard.sh -- serve TensorBoard for the wetland factorial experiment.
#
# TensorBoard is only a *reader* of the event files on disk, so it does NOT need
# to run inside the training Docker container. dl_04_train_lightning.py writes
# logs to results/<config>/seed<N>/tb_logs/<run_name>/, which lives on the host
# under /workdir/$USER (the only path the container can mount). Pointing --logdir
# at the results/ root makes every config/seed cell show up as its own run, so
# the whole factorial is monitored from one dashboard -- and new cells appear as
# run_factorial.sh reaches them.
#
# Run this from the HOST (a second screen/tmux window on the GPU node), NOT in
# the container. Then port-forward from your laptop:
#   ssh -N -L 6006:<gpu-node>:6006 ajs544@cbsulogin.biohpc.cornell.edu
# and open http://localhost:6006
#
# Usage:   run_tensorboard.sh
# Knobs (env overrides):
#   RESULTS_DIR  log root to serve     (default /workdir/$USER/nys_wetlands/results)
#   PORT         bind port             (default 6006)
#   USE_SCREEN=1 relaunch self inside a detached `screen -S tensorboard`
set -uo pipefail

RESULTS_DIR="${RESULTS_DIR:-/workdir/$USER/nys_wetlands/results}"
PORT="${PORT:-6006}"

# Optionally re-exec into a detached screen so it survives SSH disconnects.
if [[ "${USE_SCREEN:-0}" == "1" && -z "${STY:-}" ]]; then
    command -v screen >/dev/null || { echo "[error] screen not found"; exit 1; }
    echo "Launching TensorBoard in detached screen 'tensorboard'."
    echo "Reattach with: screen -r tensorboard   (detach: Ctrl-a d)"
    exec screen -dmS tensorboard env USE_SCREEN=0 RESULTS_DIR="$RESULTS_DIR" \
        PORT="$PORT" "$0"
fi

# Resolve how to launch tensorboard, no conda required (it's a pure-Python reader
# and already a declared project dependency in pyproject.toml):
#   1. tensorboard already on PATH (an activated .venv) -> use it directly
#   2. else `uv run` from the repo root -> uv syncs the project env on demand
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
if command -v tensorboard >/dev/null; then
    TB=(tensorboard)
elif command -v uv >/dev/null && [[ -f "$REPO_ROOT/pyproject.toml" ]]; then
    echo "[info] tensorboard not on PATH; using 'uv run' from $REPO_ROOT"
    TB=(uv run --project "$REPO_ROOT" tensorboard)
else
    echo "[error] no way to launch tensorboard found. Either:"
    echo "  - activate the project env:  source .venv/bin/activate, or"
    echo "  - install uv so 'uv run' can sync the project env, or"
    echo "  - make a throwaway venv:  python -m venv tb_env && tb_env/bin/pip install tensorboard"
    echo "    (RHEL gotcha: if it then dies with 'Duplicate plugins for name projector',"
    echo "     run 'rm tb_env/lib64' -- the lib64->lib symlink makes importlib.metadata"
    echo "     see every package twice. Packages physically live in lib/.)"
    exit 1
fi

[[ -d "$RESULTS_DIR" ]] || {
    echo "[error] results dir not found: $RESULTS_DIR"
    echo "  Set RESULTS_DIR to wherever training is writing results/ on the host."
    exit 1
}

echo "=============================================================="
echo " logdir: $RESULTS_DIR"
echo " url:    http://localhost:$PORT  (after SSH -L $PORT:<gpu-node>:$PORT)"
echo "=============================================================="

# --bind_all so the forwarded SSH tunnel can reach it; reload_multifile picks up
# the multiple per-cell event files cleanly as the factorial progresses.
exec "${TB[@]}" \
    --logdir "$RESULTS_DIR" \
    --port "$PORT" \
    --bind_all \
    --reload_multifile true
