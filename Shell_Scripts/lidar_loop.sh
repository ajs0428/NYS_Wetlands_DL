#!/bin/bash -l
#SBATCH --nodelist=cbsuxu09,cbsuxu10
#SBATCH --mail-user=ajs544@cornell.edu
#SBATCH --mail-type=ALL
#SBATCH --mem-per-cpu=16G
#SBATCH --cpus-per-task=4
#SBATCH --job-name=lidar
#SBATCH --ntasks=2
#SBATCH --output=Shell_Scripts/SLURM/slurm-lidar-%j.out

cd /ibstorage/anthony/NYS_Wetlands_DL/

export TMPDIR=/ibstorage/anthony/tmp

module load R/4.4.3

GPKG="Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg"
OUTDIR="Data/Lidar/Metrics"
WORKERS=4  # parallel workers per cluster (~1.5GB RAM each)

# Cluster-to-FTP-project mapping (add entries as needed)
# Format: "cluster_number|ftp_project_url"
entries=(
    "208|ftp://ftp.gis.ny.gov/elevation/LIDAR/NYSGPO_CentralFingerLakes_2020/"
)

for entry in "${entries[@]}"; do
    cluster="${entry%%|*}"
    project_url="${entry##*|}"
    echo "Running lidar metrics for cluster $cluster from $project_url"
    Rscript R_Code_Analysis/Lidar_ftp.R \
        "$GPKG" \
        "$cluster" \
        "$project_url" \
        "$OUTDIR" \
        "$WORKERS" >> "Shell_Scripts/logs/lidar_$(date +%Y%m%d).log" 2>&1
done

echo "All lidar metric extractions completed."
