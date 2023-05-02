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
# turn the national_courage score to a scale from 0 to 10 using softmax
world_courage['national_courage'] = np.exp(world_courage['national_courage']) / np.sum(np.exp(world_courage['national_courage'])) * 10
# Create a plot of the world map with courage data colored differently
fig, ax = plt.subplots(figsize=(96,48))
world_courage.plot(column='national_courage', cmap='Reds', legend=True, ax=ax)
# remove the axis 
ax.axis('off')
# make the colourbar smaller
cbar = ax.get_figure().get_axes()[1]
cbar.set_ylabel('National Courage (0-10)', fontsize=20)
cbar.set_yticklabels([i for i in range(10)], fontsize=20)
# set cbar size
cbar.set_position([0.16, 0.3, 0.1, 0.2])

# save the plot
plt.savefig('results/world_map.png', dpi=100)
