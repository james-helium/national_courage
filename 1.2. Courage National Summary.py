# this is to create a dataset with country name and corresponding summaries of national level courage
# we will get a age-gender weighted mean of national courage


# initialise

import pandas as pd

# Read the data
filtered_full_sample = pd.read_csv('data/filtered_full_sample.csv').drop('Unnamed: 0', axis=1)
un_populations = pd.read_csv('data/un_populations.csv')
# process UN reference data

un_populations = un_populations[['Location','Time','AgeGrp','PopMale','PopFemale','PopTotal']]
# only keep 2017-2021, when our study collected data
un_populations = un_populations[un_populations.Time.between(2017, 2021)]
# take the mean of 2017-2021
un_populations = un_populations.groupby(['Location','AgeGrp']).mean().reset_index()

# regourp the population into age groups 0-14, 15-19, 20-24, 25-49, 50-69, 70-99
pop_00_14 = un_populations[un_populations.AgeGrp.isin(['0-4','5-9','10-14'])]
pop_15_19 = un_populations[un_populations.AgeGrp.isin(['15-19'])]
pop_20_24 = un_populations[un_populations.AgeGrp.isin(['20-24'])]
pop_25_49 = un_populations[un_populations.AgeGrp.isin(['25-29','30-34','35-39','40-44','45-49'])]
pop_50_69 = un_populations[un_populations.AgeGrp.isin(['50-54','55-59','60-64','65-69'])]
pop_70_99 = un_populations[un_populations.AgeGrp.isin(['70-74','75-79','80-84','85-89','90-94','95-99'])]

# sum up the population of 0-14, 15-19, 20-24, 25-49, 50-69, 70-99
pop_00_14 = pop_00_14.groupby('Location').sum().reset_index()
pop_00_14['AgeGrp'] = '0-14'
pop_00_14 = pop_00_14[['Location','AgeGrp','PopMale','PopFemale','PopTotal']]
pop_15_19 = pop_15_19[['Location','AgeGrp','PopMale','PopFemale','PopTotal']]
pop_20_24 = pop_20_24[['Location','AgeGrp','PopMale','PopFemale','PopTotal']]
pop_25_49 = pop_25_49.groupby('Location').sum().reset_index()
pop_25_49['AgeGrp'] = '25-49'
pop_25_49 = pop_25_49[['Location','AgeGrp','PopMale','PopFemale','PopTotal']]
pop_50_69 = pop_50_69.groupby('Location').sum().reset_index()
pop_50_69['AgeGrp'] = '50-69'
pop_50_69 = pop_50_69[['Location','AgeGrp','PopMale','PopFemale','PopTotal']]
pop_70_99 = pop_70_99.groupby('Location').sum().reset_index()
pop_70_99['AgeGrp'] = '70-99'
pop_70_99 = pop_70_99[['Location','AgeGrp','PopMale','PopFemale','PopTotal']]
# put them back together
un_populations = pd.concat([pop_00_14,pop_15_19,pop_20_24,pop_25_49,pop_50_69,pop_70_99])

# get total population
pop_total = un_populations.groupby('Location').sum().reset_index()[['Location', 'PopTotal']]
# ratio of population in each age-sex group
un_populations = un_populations.merge(pop_total, on='Location', how='left')
un_populations.rename(columns={'PopTotal_x':'AgeTotal', 'PopTotal_y':'PopTotal'}, inplace=True)
un_populations['PopMale'] = un_populations['PopMale']/un_populations['PopTotal']
un_populations['PopFemale'] = un_populations['PopFemale']/un_populations['PopTotal']
un_populations = un_populations[['Location','AgeGrp','PopMale','PopFemale']]
# standardise country names
un_populations.loc[un_populations['Location']=='China, Hong Kong SAR', 'Location'] = 'Hong Kong'
un_populations.loc[un_populations['Location']=='China, Taiwan Province of China', 'Location'] = 'Taiwan'
un_populations.loc[un_populations['Location']=='Iran (Islamic Republic of)', 'Location'] = 'Iran'
un_populations.loc[un_populations['Location']=='Republic of Korea', 'Location'] = 'South Korea'
un_populations.loc[un_populations['Location']=='Russian Federation', 'Location'] = 'Russia'
un_populations.loc[un_populations['Location']=='Türkiye', 'Location'] = 'Turkey'
un_populations.loc[un_populations['Location']=='United Kingdom of Great Britain and Northern Ireland', 'Location'] = 'United Kingdom'
un_populations.loc[un_populations['Location']=='United States of America', 'Location'] = 'United States'
un_populations.loc[un_populations['Location']=='Venezuela (Bolivarian Republic of)', 'Location'] = 'Venezuela'
un_populations.loc[un_populations['Location']=='Viet Nam', 'Location'] = 'Vietnam'
un_populations.set_index('Location', inplace=True)
# only keep countries in our sample
un_populations = un_populations.loc[filtered_full_sample['geo_country'].unique()]
# get average courage score of each country, age-sex groups

# first, we recode the demo_age to age groups
filtered_full_sample['demo_age'] = pd.to_numeric(filtered_full_sample['demo_age'], errors='coerce')
filtered_full_sample['demo_age'] = pd.cut(filtered_full_sample['demo_age'], bins=[0,14,19,24,49,69,99], labels=['0-14','15-19','20-24','25-49','50-69','70-99'])

# summarise magic_COUR as mean, group by geo_country, demo_gender, demo_age
courage_summary = filtered_full_sample.groupby(['geo_country', 'demo_gender', 'demo_age']).mean().reset_index()
courage_summary = courage_summary[courage_summary.demo_gender=='female'].merge(
    courage_summary[courage_summary.demo_gender=='male']
    , on=['geo_country', 'demo_age'])
courage_summary = courage_summary[['geo_country', 'demo_age', 'magic_COUR_x', 'magic_COUR_y']]
courage_summary.columns = ['Location', 'AgeGrp', 'CourFemale', 'CourMale']

# merge with population data
courage_summary = courage_summary.merge(un_populations, on=['Location', 'AgeGrp'])


# calculate weighted average courage score

courage_summary.CourFemale = courage_summary.CourFemale * courage_summary.PopFemale
courage_summary.CourMale = courage_summary.CourMale * courage_summary.PopMale
courage_summary = courage_summary.groupby('Location').sum().reset_index()
courage_summary['weighted_courage'] = courage_summary.CourFemale + courage_summary.CourMale
# save to csv
courage_summary = courage_summary[['Location', 'weighted_courage']]
courage_summary.columns = ['Country', 'Weighted Average']
courage_summary.to_csv('data/national_courage.csv', index=False)