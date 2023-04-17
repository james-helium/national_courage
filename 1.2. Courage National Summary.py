# this is to create a dataset with country name and corresponding summaries of national level courage
# we will get a age-gender weighted mean of national courage


# initialise

import pandas as pd

# Read the data
filtered_full_sample = pd.read_csv('data/filtered_full_sample.csv')
un_population_total = pd.read_csv('data/un_population_total.csv')
un_population_female = pd.read_csv('data/un_population_female.csv')
un_population_male = pd.read_csv('data/un_population_male.csv')
un_population_age_sex = pd.concat([un_population_total, un_population_female, un_population_male])
un_population_age_sex.set_index('Country or Area', inplace=True)


### use the un_population_age_sex data to get the proportion of each sex-age group in each country


# filter un_population_age_sex to only have the latest Source Year for each country
latest_years = un_population_age_sex.groupby('Country or Area')['Source Year'].max()
latest_years = latest_years.reset_index()
latest_years = latest_years.rename(columns={'Source Year': 'Latest Year'}).dropna()
latest_years.set_index('Country or Area', inplace=True)

un_population_age_sex = un_population_age_sex.merge(latest_years, on='Country or Area')
un_population_age_sex = un_population_age_sex[un_population_age_sex['Source Year'] == un_population_age_sex['Latest Year']]

# get the total population for each country
total_population = un_population_age_sex.Value[
    (un_population_age_sex.Sex == 'Both Sexes') &
    (un_population_age_sex.Age == 'Total')
]

total_population = total_population.to_frame()
total_population.columns = ['Total Population']

# only keep int ages
un_population_age_sex = un_population_age_sex[
    [str(x).isdigit() for x in un_population_age_sex.Age]
]
un_population_age_sex = un_population_age_sex.astype({'Age': 'int'})

# drop the both sexes rows
un_population_age_sex = un_population_age_sex[un_population_age_sex.Sex != 'Both Sexes']

# re-code age into age groups
def age_group(age):
    if age < 15:
        return '0-14'
    elif age < 25:
        return '15-24'
    elif age < 40:
        return '25-40'
    elif age < 65:
        return '40-64'
    else:
        return '65+'

un_population_age_sex.Age = un_population_age_sex.Age.apply(age_group)

# sum the population in each age band for each country
age_sex_population = un_population_age_sex.groupby(['Country or Area', 'Sex', 'Age']).sum()
age_sex_population.drop(columns=['Source Year'], inplace=True)

# bring sex and age into columns
age_sex_population.reset_index(inplace=True)

# merge into age_sex_population
population_summary = age_sex_population.merge(total_population, on='Country or Area')

# calculate the proportion of each age group
population_summary['Proportion'] = population_summary.Value / population_summary['Total Population']

# standardise country names
population_summary.loc[population_summary['Country or Area']=='China, Hong Kong SAR', 'Country or Area'] = 'Hong Kong'
population_summary.loc[population_summary['Country or Area']=='Iran (Islamic Republic of)', 'Country or Area'] = 'Iran'
population_summary.loc[population_summary['Country or Area']=='Republic of Korea', 'Country or Area'] = 'South Korea'
population_summary.loc[population_summary['Country or Area']=='Russian Federation', 'Country or Area'] = 'Russia'
population_summary.loc[population_summary['Country or Area']=='Türkiye', 'Country or Area'] = 'Turkey'
population_summary.loc[population_summary['Country or Area']=='United Kingdom of Great Britain and Northern Ireland', 'Country or Area'] = 'United Kingdom'
population_summary.loc[population_summary['Country or Area']=='United States of America', 'Country or Area'] = 'United States'
population_summary.loc[population_summary['Country or Area']=='Venezuela (Bolivarian Republic of)', 'Country or Area'] = 'Venezuela'
population_summary.loc[population_summary['Country or Area']=='Viet Nam', 'Country or Area'] = 'Vietnam'

# create a world average proportion for each age group for countries with no data
world_average = population_summary.groupby(['Sex', 'Age']).mean()
world_average.reset_index(inplace=True)
world_average['Country or Area'] = 'World Average'
world_average.Proportion = world_average.Proportion / world_average.Proportion.sum()
# merge into population_summary
population_summary = pd.concat([population_summary, world_average])


### process the filtered_full_sample data


# recode demo_age into age groups
filtered_full_sample['age_group'] = filtered_full_sample.demo_age.apply(age_group)
# capitalise demo_gender to match the UN data
filtered_full_sample['demo_gender'] = filtered_full_sample.demo_gender.str.capitalize()

# average magic_COUR for each sex-age group in each country
courage_summary = filtered_full_sample.groupby(
    ['geo_country', 'demo_gender', 'age_group']
).magic_COUR.mean().to_frame().reset_index()


### merge all together


# standardise column names
courage_summary = courage_summary.rename(columns={
    'geo_country': 'Country',
    'demo_gender': 'Sex',
    'age_group': 'Age'
})
population_summary = population_summary.rename(columns={
    'Country or Area': 'Country'
})

# set index
courage_summary.set_index(['Country', 'Sex', 'Age'], inplace=True)
population_summary.set_index(['Country', 'Sex', 'Age'], inplace=True)

# get courage_summary countries that are not in population_summary
courage_summary_countries = set(courage_summary.index.get_level_values('Country'))
population_summary_countries = set(population_summary.index.get_level_values('Country'))
missing_countries = courage_summary_countries - population_summary_countries
# add the missing countries to population_summary, use World Average in population_summary
world_average.rename(columns={'Country or Area': 'Country'}, inplace=True)
for country in missing_countries:
    this_country = world_average.copy()
    this_country['Country'] = country
    this_country.set_index(['Country', 'Sex', 'Age'], inplace=True)
    population_summary = pd.concat([population_summary, this_country])

# merge the two dataframes
full_summary = courage_summary.merge(population_summary, left_index=True, right_index=True)
full_summary = full_summary[['magic_COUR', 'Proportion']]
full_summary.reset_index(inplace=True)


### calculate the weighted average


# adjust the proportions for all countries to add up to 1
for country in full_summary['Country'].unique():
    country_proportions = full_summary[full_summary['Country'] == country].copy()
    country_proportions.loc[:,'Proportion'] = country_proportions['Proportion'] / country_proportions['Proportion'].sum()
    full_summary.loc[full_summary['Country'] == country, 'Proportion'] = country_proportions['Proportion']

# calculate the weighted average
full_summary['Weighted Average'] = full_summary['magic_COUR'] * full_summary['Proportion']
national_courage = full_summary.groupby('Country').sum()['Weighted Average'].to_frame()

# save the data
national_courage.to_csv('data/national_courage.csv')