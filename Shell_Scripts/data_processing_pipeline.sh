#!/bin/bash
set -euo pipefail

# =============================================================================
# Master Data Processing Pipeline
# Submits all HUC data processing jobs via SLURM dependency chains.
# Run from login node: bash Shell_Scripts/data_processing_pipeline.sh
# =============================================================================

SCRIPT_DIR="Shell_Scripts"
DATE=$(date +%Y%m%d_%H%M%S)
LOG="$SCRIPT_DIR/logs/pipeline_${DATE}.log"

# --- Pre-flight checks -------------------------------------------------------
REQUIRED_SCRIPTS=(
    terrain_slp_loop.sh
    terrain_curv_loop.sh
    terrain_dmv_loop.sh
    hydro_metrics_loop.sh
    chm_loop.sh
    naip_loop.sh
    sat_gee_processing.sh
)

missing=0
for script in "${REQUIRED_SCRIPTS[@]}"; do
    if [[ ! -f "$SCRIPT_DIR/$script" ]]; then
        echo "ERROR: Missing script: $SCRIPT_DIR/$script" | tee -a "$LOG"
        missing=1
    fi
done
[[ $missing -eq 1 ]] && exit 1

mkdir -p "$SCRIPT_DIR/logs" "$SCRIPT_DIR/SLURM"

# --- Helper ------------------------------------------------------------------
submit() {
    local dep_flag=""
    if [[ -n "${2:-}" ]]; then
        dep_flag="--dependency=afterok:$2"
    fi
    local job_id
    job_id=$(sbatch --parsable $dep_flag "$1")
    echo "  Submitted $(basename "$1") -> JobID $job_id" | tee -a "$LOG"
    echo "$job_id"
}

echo "=== Data Processing Pipeline ===" | tee "$LOG"
echo "Started: $(date)" | tee -a "$LOG"
echo "" | tee -a "$LOG"

# --- Phase 1: Terrain metrics (sequential chain) ----------------------------
echo "Phase 1: Terrain metrics" | tee -a "$LOG"
SLP_ID=$(submit "$SCRIPT_DIR/terrain_slp_loop.sh")
CURV_ID=$(submit "$SCRIPT_DIR/terrain_curv_loop.sh" "$SLP_ID")
DMV_ID=$(submit "$SCRIPT_DIR/terrain_dmv_loop.sh" "$CURV_ID")

# --- Phase 2: Hydro (after all terrain) -------------------------------------
echo "Phase 2: Hydro metrics" | tee -a "$LOG"
HYDRO_ID=$(submit "$SCRIPT_DIR/hydro_metrics_loop.sh" "$DMV_ID")

# --- Phase 3: Independent data (fan out after hydro) ------------------------
echo "Phase 3: CHM / NAIP / Satellite (independent, after hydro)" | tee -a "$LOG"
CHM_ID=$(submit "$SCRIPT_DIR/chm_loop.sh" "$HYDRO_ID")
NAIP_ID=$(submit "$SCRIPT_DIR/naip_loop.sh" "$HYDRO_ID")
SAT_ID=$(submit "$SCRIPT_DIR/sat_gee_processing.sh" "$HYDRO_ID")

# --- Summary -----------------------------------------------------------------
echo "" | tee -a "$LOG"
echo "=== Pipeline submitted ===" | tee -a "$LOG"
echo "  Phase 1: slp=$SLP_ID -> curv=$CURV_ID -> dmv=$DMV_ID" | tee -a "$LOG"
echo "  Phase 2: hydro=$HYDRO_ID" | tee -a "$LOG"
echo "  Phase 3: chm=$CHM_ID | naip=$NAIP_ID | sat=$SAT_ID" | tee -a "$LOG"
echo "" | tee -a "$LOG"
echo "Monitor: squeue -u \$USER" | tee -a "$LOG"
echo "Details: sacct -j $SLP_ID,$CURV_ID,$DMV_ID,$HYDRO_ID,$CHM_ID,$NAIP_ID,$SAT_ID --format=JobID,JobName,State,Elapsed,MaxRSS" | tee -a "$LOG"
echo "Log: $LOG" | tee -a "$LOG"
