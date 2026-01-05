#!/usr/bin/env python
# coding: utf-8

# # NYS_02_rasterize_labels

# In[116]:


from pathlib import Path
import os
workdir = Path("/Users/Anthony/Data and Analysis Local/NYS_Wetlands_GHG/")
print(workdir)
os.chdir(workdir)
current_working_dir = Path.cwd()
print(f"Current working directory is now: {current_working_dir}")


# In[117]:


import rasterio
from rasterio import features
import geopandas as gpd
import numpy as np
import sys


# In[118]:


# === Testing Args ===
args = ["Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg",
        208,
        "Data/Training_Data/Wetland_Polygons_For_DL/",
        "Data/TerrainProcessed/HUC_DEMs/",
        "Data/Training_Data/DL_HUC_Extracted_Training_Data"]


# In[119]:


# === Terminal Import Args ===
# args = sys.argv


# In[120]:


# === Cluster Import ===
# ny_hucs = gpd.read_file(args[0], where=f"cluster = '{args[1]}' AND huc12 = '{args[2]}'")
aoi_hucs = gpd.read_file(args[0], where=f"cluster = '{args[1]}'")

# === CLASS MAPPING ===
class_mapping = {
    'EMW': 1,  # Emergent Wetland
    'FSW': 2,  # Forested Wetland
    'SSW': 3,  # Shrub Scrub Wetland
    'OWW': 4,  # Open Water Wetland
}


# In[ ]:




