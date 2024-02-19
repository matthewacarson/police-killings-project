# %%
import os
import glob
import geopandas as gpd
import pandas as pd
import numpy as np
from shapely.geometry import Point
# os.getcwd()

# %%
# ------------------------
# combine shape files
# ------------------------

# Search for all .shp files in the directory
directory = "C:/Users/madou/OneDrive - UCLA IT Services/1)_PS-Honors/police-killings-project_union_PC/udp_expansion_matt/data/inputs/shp"
file_paths = glob.glob(directory + '\\*.shp')

file_paths
# %%
# run in parallel
from concurrent.futures import ThreadPoolExecutor

def combine_shapefile(file_path):
    """Function to combine a single Shapefile."""
    return gpd.read_file(file_path)

# Create a ThreadPoolExecutor with max_workers set to the number of CPU cores
with ThreadPoolExecutor(max_workers=None) as executor:
    # Submit tasks to read each Shapefile asynchronously
    futures = [executor.submit(combine_shapefile, file) for file in file_paths]
    
    # Wait for all tasks to complete and retrieve the results
    shapefiles = [future.result() for future in futures]

# Combine the Shapefiles into a single GeoDataFrame
city_shp = gpd.GeoDataFrame(pd.concat(shapefiles, ignore_index=True))

# Save the combined GeoDataFrame as a new Shapefile
city_shp.to_file('combined_output.shp')
# %%
