# initialise

import pandas as pd

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
# turn the national_courage score into z-scores
world_courage['national_courage'] = world_courage['national_courage'].transform(lambda x: x**5)
world_courage['national_courage'] = (world_courage['national_courage'] - world_courage['national_courage'].mean()) / world_courage['national_courage'].std()
# Create a plot of the world map with courage data colored differently
fig, ax = plt.subplots(figsize=(192,96))
world_courage.plot(column='national_courage', cmap='Reds', legend=True, ax=ax)
# remove the axis
ax.axis('off')
# make the colourbar smaller
cbar = ax.get_figure().get_axes()[1]
cbar.set_ylabel('Z-Score', fontsize=100)
cbar.set_yticklabels(cbar.get_yticklabels(), fontsize=100)
# set cbar size
cbar.set_position([0.16, 0.3, 0.1, 0.2])

# save the plot
plt.savefig('results/world_map.png', dpi=100)
