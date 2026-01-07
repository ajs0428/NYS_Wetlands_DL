#!/usr/bin/env python
# coding: utf-8

# In[7]:


from pathlib import Path
import os
workdir = Path("/Users/Anthony/Data and Analysis Local/NYS_Wetlands_DL/")
print(workdir)
os.chdir(workdir)
current_working_dir = Path.cwd()
print(f"Current working directory is now: {current_working_dir}")


# In[8]:


import rasterio
from rasterio import features
import geopandas as gpd
import numpy as np
import sys


# In[4]:


# === Terminal Import Args ===
# args = sys.argv


# In[10]:


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

