# this script is used to extract the remaining controls
# sources: UN, OECD, WB

# initialise

import pandas as pd

# Read the data
oecd_edu_adult = pd.read_csv("data/oecd_edu_adult.csv")
oecd_gdp_per_cap = pd.read_csv("data/oecd_gdp_per_cap.csv")
oecd_urbanisation = pd.read_csv("data/oecd_urbanisation.csv")
un_edu_primary_complete = pd.read_csv("data/un_edu_primary_complete.csv")
un_edu_primary_enroll = pd.read_csv("data/un_edu_primary_enroll.csv")
un_edu_secondary_enroll = pd.read_csv("data/un_edu_secondary_enroll.csv")
un_edu_tertiary_enroll = pd.read_csv("data/un_edu_tertiary_enroll.csv")
un_gdp_per_cap = pd.read_csv("data/un_gdp_per_cap.csv")
un_urban_population = pd.read_csv("data/un_urban_population.csv")
wb_edu_all_indicators = pd.read_csv("data/wb_edu_all_indicators.csv")
wb_gdp_per_cap = pd.read_csv("data/wb_gdp_per_cap.csv")
wb_gdp_per_cap_ppp = pd.read_csv("data/wb_gdp_per_cap_ppp.csv")
wb_gdp_ppp = pd.read_csv("data/wb_gdp_ppp.csv")
wb_gdp = pd.read_csv("data/wb_gdp.csv")
wb_urban_all_indicators = pd.read_csv("data/wb_urban_all_indicators.csv")

#### process oecd data


### process oecd_edu_adult

# only keep the latest year for each country
latest_year = oecd_edu_adult.groupby("LOCATION")["TIME"].transform(max) == oecd_edu_adult["TIME"]
oecd_edu_adult = oecd_edu_adult[latest_year]
# extract 'BUPPSRY', 'TRY', 'UPPSRY' in the SUBJECT column for each country
oecd_edu_adult = oecd_edu_adult[oecd_edu_adult.SUBJECT.isin(['BUPPSRY', 'TRY', 'UPPSRY'])]
# create a new df with countries as index and columns as 'BUPPSRY', 'TRY', 'UPPSRY'
oecd_edu_adult = oecd_edu_adult.pivot(index='LOCATION', columns='SUBJECT', values='Value')

### process oecd_gdp_per_cap

# only keep the latest year for each country
latest_year = oecd_gdp_per_cap.groupby("LOCATION")["TIME"].transform(max) == oecd_gdp_per_cap["TIME"]
oecd_gdp_per_cap = oecd_gdp_per_cap[latest_year]
# create a new df with countries as index and columns as 'MLN_USD' and 'USD_CAP' in MEASURE column
oecd_gdp_per_cap = oecd_gdp_per_cap[oecd_gdp_per_cap.MEASURE.isin(['MLN_USD', 'USD_CAP'])]
oecd_gdp_per_cap = oecd_gdp_per_cap.pivot(index='LOCATION', columns='MEASURE', values='Value')

### process oecd_urbanisation

# only keep the latest year for each country
latest_year = oecd_urbanisation.groupby("LOCATION")["TIME"].transform(max) == oecd_urbanisation["TIME"]
oecd_urbanisation = oecd_urbanisation[latest_year]
# create a new df with countries as index and columns as 'URBAN' in SUBJECT column
oecd_urbanisation = oecd_urbanisation[oecd_urbanisation.SUBJECT.isin(['URBAN'])]
oecd_urbanisation = oecd_urbanisation.pivot(index='LOCATION', columns='SUBJECT', values='Value')

### merge all oecd data

oecd_data = oecd_edu_adult.merge(oecd_gdp_per_cap, left_index=True, right_index=True)
# rename columns
oecd_data.columns = [
    'oecd_edu_population_to_primary',
    'oecd_edu_population_to_tertiary',
    'oecd_edu_population_to_secondary',
    'oecd_gdp',
    'oecd_gdp_per_cap'
]



#### process un data


# a function to process un data
def process_un_edu(df, indicator: str):
    # only keep the latest year for each country
    latest_year = df.groupby("Reference Area")["Time Period"].transform(max) == df["Time Period"]
    df = df[latest_year]
    # filter sex to all genders
    df = df[df.Sex == 'All genders']
    # set Reference Area as index
    df = df.set_index('Reference Area')
    # only keep Observation Value
    df = df[['Observation Value']]
    # rename column
    df.columns = [indicator]
    return df

# process 
un_edu_primary_complete = process_un_edu(un_edu_primary_complete, 'un_edu_primary_complete')
un_edu_primary_enroll = process_un_edu(un_edu_primary_enroll, 'un_edu_primary_enroll')
un_edu_secondary_enroll = process_un_edu(un_edu_secondary_enroll, 'un_edu_secondary_enroll')
un_edu_tertiary_enroll = process_un_edu(un_edu_tertiary_enroll, 'un_edu_tertiary_enroll')

### process un_gdp_per_cap

# only keep the latest year for each country
latest_year = un_gdp_per_cap.groupby("Country or Area")["Year"].transform(max) == un_gdp_per_cap["Year"]
un_gdp_per_cap = un_gdp_per_cap[latest_year]
# set Country or Area as index
un_gdp_per_cap = un_gdp_per_cap.set_index('Country or Area')
# only keep Value
un_gdp_per_cap = un_gdp_per_cap[['Value']]
# rename column
un_gdp_per_cap.columns = ['un_gdp_per_cap']

### process un_urban_population

# drop rows with NaN
un_urban_population = un_urban_population.dropna()
# set Year to int
un_urban_population['Year'] = un_urban_population['Year'].astype(int)
# only keep the latest year for each country
latest_year = un_urban_population.groupby("Country or Area")["Year"].transform(max) == un_urban_population["Year"]
un_urban_population = un_urban_population[latest_year]
# only keep the lastest Source Year for each country
latest_source_year = un_urban_population.groupby("Country or Area")["Source Year"].transform(max) == un_urban_population["Source Year"]
un_urban_population = un_urban_population[latest_source_year]
# only keep Both Sexes in Sex column
un_urban_population = un_urban_population[un_urban_population.Sex=='Both Sexes']
# if there are repeats of country and area, keep one with the largest value
un_urban_population = un_urban_population.sort_values(by='Value', ascending=False).drop_duplicates(subset=['Country or Area', 'Area'])
# pivot to Area
un_urban_population = un_urban_population.pivot(index='Country or Area', columns='Area', values='Value')

# get urban ratio 
un_urban_population['un_urban_ratio'] = un_urban_population['Urban']/un_urban_population['Total']
un_urban_population = un_urban_population[['un_urban_ratio']]

# merge all un data
un_data = pd.concat([
    un_edu_primary_complete,
    un_edu_primary_enroll,
    un_edu_secondary_enroll,
    un_edu_tertiary_enroll,
    un_gdp_per_cap,
    un_urban_population
], axis=1)
#### process wb data


### process wb_edu_all_indicators

# a function to get the last non-na value of a list of values
def last_non_na(values):
    for value in reversed(values):
        if not pd.isna(value):
            return value
    return None
# a value column to store the last non-na value
wb_edu_all_indicators['value'] = wb_edu_all_indicators.iloc[:,4:11].apply(last_non_na, axis=1)
# pivot to get a df with country name and code as index and indicator name as columns
wb_edu_all_indicators = wb_edu_all_indicators.pivot(index=['Country Name', 'Country Code'], columns='Indicator Name', values='value')

# filter to key indicators
wb_edu_all_indicators = wb_edu_all_indicators[[
    'Literacy rate, adult total (% of people ages 15 and above)',
    'Literacy rate, youth total (% of people ages 15-24)',
    'Primary completion rate, total (% of relevant age group)',
    'School enrollment, primary (% net)',
    'School enrollment, secondary (% net)',
    'School enrollment, tertiary (% gross)'
]]
# rename columns
wb_edu_all_indicators.columns = [
    'wb_edu_literacy_adult',
    'wb_edu_literacy_youth',
    'wb_edu_primary_complete',
    'wb_edu_primary_enroll',
    'wb_edu_secondary_enroll',
    'wb_edu_tertiary_enroll'
]

### process wb_gdp_per_cap and wb_gdp

# concat the two
wb_gdps = pd.concat([wb_gdp_per_cap, wb_gdp], axis=0)
# a value column to store the last non-na value
wb_gdps['value'] = wb_gdps.iloc[:,4:11].apply(last_non_na, axis=1)
# pivot to get a df with country name and code as index and indicator name as columns
wb_gdps = wb_gdps.pivot(index=['Country Name', 'Country Code'], columns='Indicator Name', values='value')
# rename
wb_gdps.columns = ['wb_gdp_per_cap', 'wb_gdp']

### process wb_gdp_per_cap_ppp and wb_gdp_ppp

def process_wb_ppp(df, name):
    df = df.copy()
    # filter to digit Years
    df = df[df.Year.apply(lambda x: str(x).isdigit())]
    # set Year to int
    df['Year'] = df['Year'].astype(int)
    # only keep the latest year for each country
    latest_year = df.groupby("Country or Area")["Year"].transform(max) == df["Year"]
    df = df[latest_year]
    # set Country or Area as index
    df = df.set_index('Country or Area')
    # only keep Value
    df = df[['Value']]
    # rename column
    df.columns = [name]
    return df

# process
wb_gdp_per_cap_ppp = process_wb_ppp(wb_gdp_per_cap_ppp, 'wb_gdp_per_cap_ppp')
wb_gdp_ppp = process_wb_ppp(wb_gdp_ppp, 'wb_gdp_ppp')

### process wb_urban_all_indicators

# a value column to store the last non-na value
wb_urban_all_indicators['value'] = wb_urban_all_indicators.iloc[:,4:11].apply(last_non_na, axis=1)
# pivot to get a df with country name and code as index and indicator name as columns
wb_urban_all_indicators = wb_urban_all_indicators.pivot(index=['Country Name', 'Country Code'], columns='Indicator Name', values='value')

# filter to key indicators
wb_urban_all_indicators = wb_urban_all_indicators[[
    'Population density (people per sq. km of land area)',
    'Population in the largest city (% of urban population)',
    'Population in urban agglomerations of more than 1 million (% of total population)',
    'Urban population (% of total population)'
]]
# rename columns
wb_urban_all_indicators.columns = [
    'wb_urban_density',
    'wb_urban_largest_city_ratio',
    'wb_urban_big_city_ratio',
    'wb_urban_population_ratio'
]

# country name and code
country_name_code = wb_gdps.index.to_frame().reset_index(drop=True)

# rename wb ppps 'Country or Area' to 'Country Name'
wb_gdp_per_cap_ppp = wb_gdp_per_cap_ppp.rename_axis('Country Name').reset_index()
wb_gdp_ppp = wb_gdp_ppp.rename_axis('Country Name').reset_index()
# add country code to wb pps 
wb_gdp_per_cap_ppp = wb_gdp_per_cap_ppp.reset_index().merge(country_name_code, on='Country Name').set_index(['Country Name', 'Country Code'])
wb_gdp_ppp = wb_gdp_ppp.reset_index().merge(country_name_code, on='Country Name').set_index(['Country Name', 'Country Code'])
# drop column 'index'
wb_gdp_per_cap_ppp = wb_gdp_per_cap_ppp.drop(columns=['index'])
wb_gdp_ppp = wb_gdp_ppp.drop(columns=['index'])

# merge all wb data
wb_data = pd.concat([
    wb_edu_all_indicators,
    wb_gdps,
    wb_gdp_per_cap_ppp,
    wb_gdp_ppp,
    wb_urban_all_indicators
], axis=1)
#### merge all data

# use country name and code to add country name to oecd data
oecd_data = oecd_data.rename_axis('Country Code').reset_index()
oecd_data = oecd_data.reset_index().merge(country_name_code, on='Country Code').set_index(['Country Name', 'Country Code'])
oecd_data = oecd_data.drop('index', axis=1)

# do the same to un data
un_data = un_data.rename_axis('Country Name').reset_index()
un_data = un_data.reset_index().merge(country_name_code, on='Country Name').set_index(['Country Name', 'Country Code'])
un_data = un_data.drop('index', axis=1)

# merge all data
all_data = pd.concat([
    oecd_data,
    un_data,
    wb_data
], axis=1)

# save to csv
all_data.to_csv('data/main_controls_extract.csv')

