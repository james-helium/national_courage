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
country_names = main_controls[
    ["Country Name", "Country Code"]
].drop_duplicates()


# standadise national courange country names and code
national_courage = national_courage.merge(
    country_names, left_on="Country", right_on="Country Name", how="left"
)
national_courage.loc[
    national_courage["Country"] == "Egypt", "Country Code"
] = "EGY"
national_courage.loc[
    national_courage["Country"] == "Hong Kong", "Country Code"
] = "HKG"
national_courage.loc[
    national_courage["Country"] == "Iran", "Country Code"
] = "IRN"
national_courage.loc[
    national_courage["Country"] == "South Korea", "Country Code"
] = "KOR"
national_courage.loc[
    national_courage["Country"] == "Taiwan", "Country Code"
] = "TWN"
national_courage.loc[
    national_courage["Country"] == "Russia", "Country Code"
] = "RUS"
national_courage.loc[
    national_courage["Country"] == "Slovakia", "Country Code"
] = "SVK"
national_courage.loc[
    national_courage["Country"] == "Turkey", "Country Code"
] = "TUR"
national_courage.loc[
    national_courage["Country"] == "Venezuela", "Country Code"
] = "VEN"

national_courage = national_courage.drop("Country Name", axis=1)
national_courage = national_courage.rename(columns={"Country": "Country Name"})
national_courage = national_courage.set_index(["Country Name", "Country Code"])
national_courage.columns = ["national_courage", "national_courage_unweighted"]


# standardise ipums controls with country names and code
ipums_controls = ipums_controls.merge(
    country_names, left_on="Country", right_on="Country Name", how="left"
)
ipums_controls.loc[
    ipums_controls["Country"] == "Egypt", "Country Code"
] = "EGY"
ipums_controls.loc[ipums_controls["Country"] == "Iran", "Country Code"] = "IRN"
ipums_controls.loc[
    ipums_controls["Country"] == "Russia", "Country Code"
] = "RUS"
ipums_controls.loc[
    ipums_controls["Country"] == "Turkey", "Country Code"
] = "TUR"
ipums_controls.loc[
    ipums_controls["Country"] == "Venezuela", "Country Code"
] = "VEN"

ipums_controls = ipums_controls.drop("Country Name", axis=1)
ipums_controls = ipums_controls.rename(columns={"Country": "Country Name"})
ipums_controls = ipums_controls.set_index(["Country Name", "Country Code"])
ipums_controls.columns = [
    "index",
    "ipums_edu_tertiary_attain",
    "ipums_edu_adult_literacy",
    "ipums_urbanisation",
]
ipums_controls = ipums_controls.drop("index", axis=1)


# standardise ipums idv with country names and code in ipums_controls
ipums_idv = ipums_idv.merge(
    ipums_controls.index.to_frame().reset_index(drop=True),
    left_on="Country",
    right_on="Country Name",
    how="left",
)
ipums_idv.loc[ipums_idv["Country"] == "Slovakia", "Country Code"] = "SVK"
ipums_idv.loc[ipums_idv["Country"] == "Slovenia", "Country Code"] = "SVN"

ipums_idv = ipums_idv.drop("Country Name", axis=1)
ipums_idv = ipums_idv.rename(columns={"Country": "Country Name"})
ipums_idv = ipums_idv.set_index(["Country Name", "Country Code"])
ipums_idv.columns = ["index", "ipums_idv"]
ipums_idv = ipums_idv.drop("index", axis=1)


# standardise wvs idv with country names and code
wvs_idv = wvs_idv.merge(
    country_names, left_on="Country", right_on="Country Code", how="left"
)
wvs_idv.loc[wvs_idv["Country"] == "TWN", "Country Name"] = "Taiwan"
wvs_idv.loc[wvs_idv["Country"] == "NIR", "Country Name"] = "Northern Ireland"
wvs_idv = wvs_idv.drop("Country Code", axis=1)
wvs_idv = wvs_idv.rename(columns={"Country": "Country Code"})
wvs_idv = wvs_idv.set_index(["Country Name", "Country Code"])
wvs_idv.columns = ["index", "wvs_idv"]
wvs_idv = wvs_idv.drop("index", axis=1)


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
    [national_courage, main_controls, ipums_controls, ipums_idv, wvs_idv],
    axis=1,
)
all_national_data = all_national_data.merge(
    country_names, left_index=True, right_on="Country Code", how="left"
)
all_national_data = all_national_data.drop("index", axis=1)
all_national_data = all_national_data[
    all_national_data["Country Code"].isin(
        national_courage.index.get_level_values(0)
    )
]


# set country name and country code as index
all_national_data = all_national_data.merge(
    manual_data, on=["Country Name", "Country Code"], how="left"
)
all_national_data = all_national_data.set_index(
    ["Country Name", "Country Code"]
)
# additional data
allik_big_five = pd.read_csv("data/allik_big_five.csv")
global_giving_index = pd.read_csv("data/global_giving_index.csv")
globe_measures_clean = pd.read_csv("data/globe_measures_clean.csv")
self_esteem = pd.read_csv("data/self_esteem.csv")
# clean additional data
allik_big_five["CODE"] = allik_big_five["CODE"].str.split("(", expand=True)[0]
allik_big_five = allik_big_five.groupby("CODE").mean()
allik_big_five = allik_big_five[["O", "C", "E", "A", "N"]]
allik_big_five.columns = [
    "allik_big_five_openness",
    "allik_big_five_conscientiousness",
    "allik_big_five_extraversion",
    "allik_big_five_agreeableness",
    "allik_big_five_neuroticism",
]
allik_big_five = allik_big_five.merge(
    country_names, left_index=True, right_on="Country Code", how="right"
).set_index(["Country Name", "Country Code"])
global_giving_index = (
    global_giving_index.merge(
        country_names, left_on="COUNTRY", right_on="Country Name", how="right"
    )
    .set_index(["Country Name", "Country Code"])[["Rank"]]
    .rename(columns={"Rank": "global_giving_index_rank"})
)
globe_measures_clean = globe_measures_clean.merge(
    country_names, left_on="Country Name", right_on="Country Name", how="right"
).set_index(["Country Name", "Country Code"])
globe_measures_clean.columns = [
    "GLOBE_" + "_".join(str(col).lower().split(" "))
    for col in globe_measures_clean.columns
]
self_esteem = self_esteem.merge(
    country_names, left_on="Country", right_on="Country Name", how="right"
).set_index(["Country Name", "Country Code"])[["Rosenberg Self-Esteem"]]
self_esteem.columns = ["rosenberg_self_esteem"]
# merge in additional data
all_national_data = pd.concat(
    [
        all_national_data,
        allik_big_five,
        global_giving_index,
        globe_measures_clean,
        self_esteem,
    ],
    axis=1,
)
all_national_data.to_csv("data/all_national_data.csv")
