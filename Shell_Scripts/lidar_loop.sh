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
INDEX_DIR="Data/Lidar/Indexes"

# Cluster-to-index mapping (add entries as needed)
# Format: "cluster_number|index_gpkg_filename"
entries=(
    "208|NYS_Central_Finger_Lakes_2020.gpkg"
)

for entry in "${entries[@]}"; do
    cluster="${entry%%|*}"
    index_file="${entry##*|}"
    echo "Running lidar metrics for cluster $cluster using $index_file"
    Rscript R_Code_Analysis/Lidar_ftp.R \
        "$GPKG" \
        "$cluster" \
        "$INDEX_DIR/$index_file" \
        "$OUTDIR" >> "Shell_Scripts/logs/lidar_$(date +%Y%m%d).log" 2>&1
done

echo "All lidar metric extractions completed."
