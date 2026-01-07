#!/usr/bin/env python
# coding: utf-8

# In[10]:


from pathlib import Path
import os
workdir = Path("/Users/Anthony/Data and Analysis Local/NYS_Wetlands_DL/")
print(workdir)
os.chdir(workdir)
current_working_dir = Path.cwd()
print(f"Current working directory is now: {current_working_dir}")


# In[11]:


import rasterio
import geopandas as gpd
import numpy as np
from pathlib import Path
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
import json


# In[13]:


# === Cluster Import ===
ny_hucs = gpd.read_file(args[0], where=f"cluster = '{args[1]}' AND huc12 LIKE '{args[2]}'")
ny_hucs


# In[14]:


# === CONFIGURATION ===

patch_size = 128
max_offset = 32  # Random offset from centroid (pixels) to add variety
background_patches = 120  # Number of random background patches to include
val_split = 0.2
random_seed = 42

output_dir = Path("Data/Patches_v2")


# In[ ]:





# In[ ]:




