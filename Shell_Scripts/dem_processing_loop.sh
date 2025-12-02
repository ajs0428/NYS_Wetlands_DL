#!/bin/bash -l
#SBATCH --nodelist=cbsuxu09
#SBATCH --mail-user=ajs544@cornell.edu
#SBATCH --mail-type=ALL
#SBATCH --mem-per-cpu=24G
#SBATCH --cpus-per-task=6
#SBATCH --job-name=dem_processing
#SBATCH --ntasks=1
#SBATCH --output=Shell_Scripts/SLURM/slurm-dems-%j.out

cd /ibstorage/anthony/NYS_Wetlands_GHG/

export TMPDIR=/ibstorage/anthony/tmp

module load R/4.4.3


# Define the list of numbers
all=(1 2 3 4 5 6 7 8 9 10 13 14 15 16 17 18 19 20 21 23 24 25 26 27 28 29 30 31 32 33 34 35 36 \
37 38 39 40 41 42 43 44 45 46 47 48 49 50 52 54 55 57 58 59 61 62 63 65 66 68 69 70 71 72 73 74 75 \
76 77 78 79 80 81 82 83 85 87 88 89 91 93 94 95 96 97 98 99 100 101 103 104 106 107 108 109 110 111 112 113 114 \
115 117 118 119 121 122 124 125 126 127 128 129 130 131 132 133 134 135 137 139 140 141 142 143 144 145 146 147 148 149 150 151 153 \
154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 177 178 179 180 181 182 184 185 186 187 188 \
190 191 194 195 196 197 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 219 220 221 222 223 224 226 227 \
228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249)
# include=(11  12  22  51  53  56  60  64  67  84  86  90  92 102 105 116 120 123 136 138 152 176 183 189 192 193 198 218 225 250)
include=(92 120 123 189 198)
# Loop through each number in the list
for number in "${include[@]}"; do
    echo "Running Rscript with argument: $number"
    
    Rscript R_Code_Analysis/DEM_Extract_singleVect_CMD.r \
        "Data/NYS_DEM_Indexes/" \
        "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg" \
        "$number" \
        "Data/DEMs/" \
        "Data/TerrainProcessed/HUC_DEMs/" >> "Shell_Scripts/logs/dem_processing_loop.log" 2>&1
    
    if [ $? -eq 0 ]; then
        echo "Successfully completed Rscript for number: $number"
    else
        echo "Error: Rscript failed for number: $number"
    fi
    echo "---"
done


