#!/usr/bin/env bash
# Build the NWI wetlands PMTiles overlay for the web map (COG_Plan.md Phase 2b).
#
# Streams the NY_Wetlands layer (915k polygons, EPSG:6347) out of the NWI
# geopackage, reprojects to WGS84, and tiles it into a single static
# nwi.pmtiles that the Leaflet viewer reads by HTTP range request (same
# no-tile-server model as the COGs).
#
# CPU-only, login-node friendly (~10-30 min single core). Run from anywhere:
#   bash Shell_Scripts/make_nwi_pmtiles.sh
set -euo pipefail

NWI_GPKG=/ibstorage/anthony/NYS_Wetlands_Data/Data/NWI/NY_NWI_6347.gpkg
NWI_LAYER=NY_Wetlands
OUT=/ibstorage/anthony/NYS_Wetlands_DL/Data/HUC_DL_Predictions_v2/nwi.pmtiles

# tool paths (node-local conda envs; tippecanoe kept out of wetland-cnn on purpose)
OGR2OGR=/workdir/$USER/miniconda3/envs/wetland-cnn/bin/ogr2ogr
TIPPECANOE=/workdir/$USER/miniconda3/envs/tippecanoe/bin/tippecanoe
[[ -x $OGR2OGR && -x $TIPPECANOE ]] || { echo "missing ogr2ogr or tippecanoe" >&2; exit 1; }

echo "[$(date +%T)] exporting $NWI_LAYER -> GeoJSONSeq -> tippecanoe"
# build to a temp name and rename at the end: tippecanoe pre-creates its
# output file, and the viewer HEAD-checks this path to decide availability
"$OGR2OGR" -f GeoJSONSeq /vsistdout/ \
    -t_srs EPSG:4326 \
    -select ATTRIBUTE,WETLAND_TYPE \
    "$NWI_GPKG" "$NWI_LAYER" |
"$TIPPECANOE" -o "$OUT.building" --force \
    --layer=nwi --name="NWI wetlands (NY)" \
    --minimum-zoom=5 -zg \
    --coalesce-densest-as-needed --extend-zooms-if-still-dropping \
    --simplification=8
mv "$OUT.building" "$OUT"

echo "[$(date +%T)] done:"
ls -lh "$OUT"
