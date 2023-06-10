# this script combines the data sources into one data frame
##### IMPORTANT: hofstede, terrorism, innovation data are manually copied from books and websites #####

# initialise

import pandas as pd

# Read the data
national_courage = pd.read_csv("data/national_courage.csv")
main_controls = pd.read_csv("data/main_controls_extract.csv")
ipums_controls = pd.read_csv("data/ipums_controls_extract.csv")
ipums_idv = pd.read_csv("data/ipums_idv_extract.csv")
wvs_idv = pd.read_csv("data/wvs_idv_extract.csv")
manual_data = pd.read_csv("data/manual_data.csv")
# get country names and code from main controls
country_names = main_controls[["Country Name", "Country Code"]].drop_duplicates()
# standadise national courange country names and code
national_courage = national_courage.merge(
    country_names, left_on="Country", right_on="Country Name", how="left"
)
national_courage.loc[national_courage["Country"] == "Egypt", "Country Code"] = "EGY"
national_courage.loc[national_courage["Country"] == "Hong Kong", "Country Code"] = "HKG"
national_courage.loc[national_courage["Country"] == "Iran", "Country Code"] = "IRN"
national_courage.loc[
    national_courage["Country"] == "South Korea", "Country Code"
] = "KOR"
national_courage.loc[national_courage["Country"] == "Taiwan", "Country Code"] = "TWN"
national_courage.loc[national_courage["Country"] == "Russia", "Country Code"] = "RUS"
national_courage.loc[national_courage["Country"] == "Slovakia", "Country Code"] = "SVK"
national_courage.loc[national_courage["Country"] == "Turkey", "Country Code"] = "TUR"
national_courage.loc[national_courage["Country"] == "Venezuela", "Country Code"] = "VEN"

# drop country name
national_courage = national_courage.drop("Country Name", axis=1)
# rename Country to Country Name
national_courage = national_courage.rename(columns={"Country": "Country Name"})
# set Country and Country Code as index
national_courage = national_courage.set_index(["Country Name", "Country Code"])
# rename columns
national_courage.columns = ["national_courage", "national_courage_unweighted"]
# standardise ipums controls with country names and code
ipums_controls = ipums_controls.merge(
    country_names, left_on="Country", right_on="Country Name", how="left"
)
# mannually set Country Code for missing values: 'Egypt', 'Iran', 'Russia', 'Turkey', 'Venezuela'
ipums_controls.loc[ipums_controls["Country"] == "Egypt", "Country Code"] = "EGY"
ipums_controls.loc[ipums_controls["Country"] == "Iran", "Country Code"] = "IRN"
ipums_controls.loc[ipums_controls["Country"] == "Russia", "Country Code"] = "RUS"
ipums_controls.loc[ipums_controls["Country"] == "Turkey", "Country Code"] = "TUR"
ipums_controls.loc[ipums_controls["Country"] == "Venezuela", "Country Code"] = "VEN"

# drop country name
ipums_controls = ipums_controls.drop("Country Name", axis=1)
# rename Country to Country Name
ipums_controls = ipums_controls.rename(columns={"Country": "Country Name"})
# set Country and Country Code as index
ipums_controls = ipums_controls.set_index(["Country Name", "Country Code"])
# rename columns
ipums_controls.columns = [
    "index",
    "ipums_edu_tertiary_attain",
    "ipums_edu_adult_literacy",
    "ipums_urbanisation",
]
# drop index
ipums_controls = ipums_controls.drop("index", axis=1)
# standardise ipums idv with country names and code in ipums_controls
ipums_idv = ipums_idv.merge(
    ipums_controls.index.to_frame().reset_index(drop=True),
    left_on="Country",
    right_on="Country Name",
    how="left",
)
# manually add slovakia and slovenia
ipums_idv.loc[ipums_idv["Country"] == "Slovakia", "Country Code"] = "SVK"
ipums_idv.loc[ipums_idv["Country"] == "Slovenia", "Country Code"] = "SVN"

# drop country name
ipums_idv = ipums_idv.drop("Country Name", axis=1)
# rename Country to Country Name
ipums_idv = ipums_idv.rename(columns={"Country": "Country Name"})
# set Country and Country Code as index
ipums_idv = ipums_idv.set_index(["Country Name", "Country Code"])
# rename columns
ipums_idv.columns = ["index", "ipums_idv"]
# drop index
ipums_idv = ipums_idv.drop("index", axis=1)
# standardise wvs idv with country names and code
wvs_idv = wvs_idv.merge(
    country_names, left_on="Country", right_on="Country Code", how="left"
)
# manually set the Country Name of TWN and NIR
wvs_idv.loc[wvs_idv["Country"] == "TWN", "Country Name"] = "Taiwan"
wvs_idv.loc[wvs_idv["Country"] == "NIR", "Country Name"] = "Northern Ireland"

# drop country code
wvs_idv = wvs_idv.drop("Country Code", axis=1)
# rename Country to Country Code
wvs_idv = wvs_idv.rename(columns={"Country": "Country Code"})
# set Country and Country Code as index
wvs_idv = wvs_idv.set_index(["Country Name", "Country Code"])
# rename columns
wvs_idv.columns = ["index", "wvs_idv"]
# drop index
wvs_idv = wvs_idv.drop("index", axis=1)
# merge all data frames

# set the index of national courage as the standard country name
country_names = national_courage.index.to_frame().reset_index(drop=True)

# set country code as the index of all data frames
main_controls = main_controls.reset_index().set_index("Country Code")
ipums_controls = ipums_controls.reset_index().set_index("Country Code")
ipums_idv = ipums_idv.reset_index().set_index("Country Code")
wvs_idv = wvs_idv.reset_index().set_index("Country Code")
national_courage = national_courage.reset_index().set_index("Country Code")
# drop all country names
main_controls = main_controls.drop("Country Name", axis=1)
ipums_controls = ipums_controls.drop("Country Name", axis=1)
ipums_idv = ipums_idv.drop("Country Name", axis=1)
wvs_idv = wvs_idv.drop("Country Name", axis=1)
national_courage = national_courage.drop("Country Name", axis=1)

# merge all data frames
all_national_data = pd.concat(
    [main_controls, ipums_controls, ipums_idv, wvs_idv, national_courage], axis=1
)
# add country names back to the data frame
all_national_data = all_national_data.merge(
    country_names, left_index=True, right_on="Country Code", how="left"
)
# drop column index
all_national_data = all_national_data.drop("index", axis=1)

# only keep the countries that are in national_courage
all_national_data = all_national_data[
    all_national_data["Country Code"].isin(national_courage.index.get_level_values(0))
]

# set country name and country code as index
all_national_data = all_national_data.merge(
    manual_data, on=["Country Name", "Country Code"], how="left"
)
all_national_data = all_national_data.set_index(["Country Name", "Country Code"])

all_national_data.to_csv("data/all_national_data.csv")
