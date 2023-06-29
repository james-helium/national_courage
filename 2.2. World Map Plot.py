# initialise

import pandas as pd
import numpy as np

# Read the data
all_national_data = pd.read_csv('data/all_national_data.csv')

import geopandas as gpd
import matplotlib.pyplot as plt

# Read in the world shapefile from geopandas datasets
world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
world.rename(columns={'iso_a3': 'Country Code'}, inplace=True)

# Merge the courage data with the world shapefile using the 'name' column
world_courage = world.merge(
    all_national_data, 
    how='left', 
    on='Country Code'
)

# transform the scale for easier visualization
world_courage['national_courage'] = world_courage['national_courage'] ** 5
# Create a plot of the world map with courage data colored differently
fig, ax = plt.subplots(figsize=(96,48))
world_courage.plot(column='national_courage', cmap='Reds', legend=False, ax=ax)
# remove the axis 
ax.axis('off')

# save the plot
plt.savefig('results/world_map.png', dpi=100)