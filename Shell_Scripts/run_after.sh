#!/usr/bin/env bash
# run_after.sh -- wait for the currently-running container(s) to exit, then run a command.
#
# The GPU is a single resource: two prediction/training arms must not overlap. This
# lets you QUEUE the next arm at any point after the first has already started,
# without babysitting the terminal:
#
#   tmux new -s armB 'bash Shell_Scripts/run_after.sh \
#     bash Shell_Scripts/run_predict_arms.sh mbfusion binary 2>&1 \
#     | tee -a /workdir/$USER/armB_binary.log'
#
# With no CONTAINER set it snapshots whichever containers of $IMAGE are running RIGHT
# NOW and waits for all of them to disappear -- so start it while the first arm is
# running and it keys onto that arm automatically. Containers started later (by
# someone else, or by the queued command itself) are ignored.
#
# If nothing is running when it starts, the command runs immediately -- so the same
# invocation is safe whether or not you beat the first arm to the terminal.
#
# Usage:  [CONTAINER=<name>] [IMAGE=nys-wetlands-dl] [POLL=120] run_after.sh <cmd> [args...]
# Env:    CONTAINER  wait for exactly this container name (else: all current $IMAGE ones)
#         IMAGE      image whose containers to watch (default nys-wetlands-dl)
#         POLL       seconds between checks (default 120)
#         HEARTBEAT  seconds between "still waiting" lines (default 1800)
set -uo pipefail

IMAGE="${IMAGE:-nys-wetlands-dl}"
POLL="${POLL:-120}"
HEARTBEAT="${HEARTBEAT:-1800}"

(( $# )) || { echo "usage: run_after.sh <command> [args...]"; exit 2; }

running_names() { docker1 ps --format '{{.Names}}\t{{.Image}}' 2>/dev/null; }

# --- Snapshot what we are waiting for. ---------------------------------------
if [[ -n "${CONTAINER:-}" ]]; then
    WAIT_FOR=("$CONTAINER")
else
    mapfile -t WAIT_FOR < <(running_names | awk -F'\t' -v img="$IMAGE" '$2 ~ img {print $1}')
fi

if (( ${#WAIT_FOR[@]} == 0 )); then
    echo "[$(date '+%F %T')] nothing running to wait for -- starting immediately."
else
    echo "[$(date '+%F %T')] waiting for: ${WAIT_FOR[*]}"
    start=$(date +%s); last=$start
    while :; do
        alive=()
        mapfile -t now < <(running_names | cut -f1)
        for w in "${WAIT_FOR[@]}"; do
            for n in "${now[@]}"; do [[ "$n" == "$w" ]] && alive+=("$w") && break; done
        done
        (( ${#alive[@]} == 0 )) && break
        sleep "$POLL"
        t=$(date +%s)
        if (( t - last >= HEARTBEAT )); then
            printf '[%s] still waiting on %s (%dh %dm elapsed)\n' "$(date '+%F %T')" \
                "${alive[*]}" $(( (t-start)/3600 )) $(( ((t-start)%3600)/60 ))
            last=$t
        fi
    done
    t=$(date +%s)
    printf '[%s] done waiting after %dh %dm.\n' "$(date '+%F %T')" \
        $(( (t-start)/3600 )) $(( ((t-start)%3600)/60 ))
fi

echo "[$(date '+%F %T')] running: $*"
echo
exec "$@"
