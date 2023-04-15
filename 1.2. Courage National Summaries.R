#this is to create a dataset with country name and corresponding summaries of national level courage
#we will create a normal average, variance, age weighed average, gender weighed average, age-gender weighed


#############################
#LIBRARIES AND FILES
#############################

library(ggplot2)
library(readxl)

Raw_Data <- read.csv("Data/Filtered Raw Data.csv")[,-1]
Age_Ratio_Total <- read_excel('External Data/UN Population/Age_Ratio_Total.xlsx')
Age_Ratio_Male <- read_excel('External Data/UN Population/Age_Ratio_Male.xlsx')
Age_Ratio_Female <- read_excel('External Data/UN Population/Age_Ratio_Female.xlsx')
Sex_Ratio <- read_excel('External Data/UN Population/Sex_Ratio.xlsx')


#############################
#EXECUTION
#############################

################# prepare main data
#we only need the courage scores, country, and demographic info, so we will just keep those
MainData <- data.frame(Country = Raw_Data$geo_country,
                       Courage = Raw_Data$magic_COUR,
                       Age = Raw_Data$demo_age,
                       Sex = Raw_Data$demo_gender)

################# filter demographic weighting data and turn age band from cumulative to discrete
#write a function to standardise country names
standardise_country_names <- function(Frame){
  Frame$Country[Frame$Country == 'China, Hong Kong SAR'] <- 'Hong Kong'
  Frame$Country[Frame$Country == 'China, Taiwan Province of China'] <- 'Taiwan'
  Frame$Country[Frame$Country == 'Iran (Islamic Republic of)'] <- 'Iran'
  Frame$Country[Frame$Country == 'Republic of Korea'] <- 'South Korea'
  Frame$Country[Frame$Country == 'Russian Federation'] <- 'Russia'
  Frame$Country[Frame$Country == 'United States of America'] <- 'United States'
  Frame$Country[Frame$Country == 'Venezuela (Bolivarian Republic of)'] <- 'Venezuela'
  Frame$Country[Frame$Country == 'Viet Nam'] <- 'Vietnam'
  #output
  Frame
}
#write a function to do this cuz the three data files have identical structures - pre-treated upon download
age_data_prep <- function(Frame){
  #first get rid of all year group data apart from 2020
  Frame <- Frame[Frame$`Reference date (as of 1 July)` == 2020,]
  #and also all entry that are not type country/area
  Frame <- Frame[Frame$Type == 'Country/Area',]
  #standardise country names
  Frame <- standardise_country_names(Frame)
  #make sure that frame data is numeric
  Frame <- data.frame(Frame)
  for(col in 4:10){Frame[,col] <- as.numeric(Frame[,col])}
  #process age bands
  band_00_18 <- Frame$X11.18
  band_19_24 <- Frame$X18. - Frame$X25.
  band_25_49 <- Frame$X25. - Frame$X50.
  band_50_69 <- Frame$X50. - Frame$X70.
  band_70_98 <- Frame$X70.
  #output processed frame
  output <- data.frame(Country = Frame$Country, band_00_18, band_19_24, band_25_49, band_50_69, band_70_98)
  output
}
#execute these function
Sex_Ratio <- standardise_country_names(Sex_Ratio)
Sex_Ratio <- data.frame(Country = Sex_Ratio$Country, male_per_100_female = Sex_Ratio$`2020`)
Age_Ratio_Total <- age_data_prep(Age_Ratio_Total)
Age_Ratio_Male <- age_data_prep(Age_Ratio_Male)
Age_Ratio_Female <- age_data_prep(Age_Ratio_Female)

################# summarise courage data through unweighed and weighed means and variances
#create a list of countries - there are 80 of them, in alphabetical order as well for standardization
Country_Names <- unique(MainData$Country)[order(unique(MainData$Country))]
#now iterate through all 80 countries to create a list of average values, also split into income groups
#empty dataframe to populate
National_Data <- data.frame(matrix(data = NA, nrow = length(Country_Names), ncol = 10))
colnames(National_Data) <- c('Country', 'Courage.Mean.Unweighted', 'Courage.Var.Unweighted', 
                             'Courage.Mean.Weighted.Age', 'Courage.Mean.Weighted.Sex', 'Courage.Mean.Weighted.Age.Sex',
                             'Courage.Mean.Male.Unweighted', 'Courage.Mean.Female.Unweighted',
                             'Courage.Mean.Male.Weighted.Age', 'Courage.Mean.Female.Weighted.Age')
National_Data$Country <- Country_Names
#iterate and populate
for(i in 1:nrow(National_Data)){
  country <- National_Data$Country[i]
  #make a dataset for each country to reduce run-time
  ThisCountry <- subset(MainData, MainData$Country == country)
  #calculate the mean & variance, unweighted
  National_Data$Courage.Mean.Unweighted[i] <- mean(ThisCountry$Courage)
  National_Data$Courage.Var.Unweighted[i] <- var(ThisCountry$Courage)
  #unweighted but split to two genders
  National_Data$Courage.Mean.Female.Unweighted[i] <- mean(ThisCountry$Courage[ThisCountry$Sex == 'female'])
  National_Data$Courage.Mean.Male.Unweighted[i] <- mean(ThisCountry$Courage[ThisCountry$Sex == 'male'])
  
  #weighted by gender
  National_Data$Courage.Mean.Weighted.Sex[i] <- 
    (mean(ThisCountry$Courage[ThisCountry$Sex == 'female']) * 100 + 
       mean(ThisCountry$Courage[ThisCountry$Sex == 'male']) * Sex_Ratio$male_per_100_female[Sex_Ratio$Country == country]) / 
    (100 + Sex_Ratio$male_per_100_female[Sex_Ratio$Country == country])
  
  #weighted by age
  National_Data$Courage.Mean.Weighted.Age[i] <- 
    mean(ThisCountry$Courage[ThisCountry$Age <= 18]) * Age_Ratio_Total$band_00_18[Age_Ratio_Total$Country == country] / 100 + 
    mean(ThisCountry$Courage[ThisCountry$Age >= 19 & ThisCountry$Age <= 24]) * 
    Age_Ratio_Total$band_19_24[Age_Ratio_Total$Country == country] / 100 + 
    mean(ThisCountry$Courage[ThisCountry$Age >= 25 & ThisCountry$Age <= 49]) * 
    Age_Ratio_Total$band_25_49[Age_Ratio_Total$Country == country] / 100 + 
    #there are countries without participants above 49, so if else check here
    ifelse(any(ThisCountry$Age >= 50 & ThisCountry$Age <= 69), 
           #if there are, we weight accordingly
           mean(ThisCountry$Courage[ThisCountry$Age >= 50 & ThisCountry$Age <= 69]) * 
             Age_Ratio_Total$band_50_69[Age_Ratio_Total$Country == country] / 100,
           #if there aren't, we extrapolate by national average (to counter the influence of younger as much as we can)
           mean(ThisCountry$Courage) * Age_Ratio_Total$band_50_69[Age_Ratio_Total$Country == country] / 100) + 
    #do the same for those above 69
    ifelse(any(ThisCountry$Age >= 70),
           mean(ThisCountry$Courage[ThisCountry$Age >= 70]) * Age_Ratio_Total$band_70_98[Age_Ratio_Total$Country == country] / 100,
           mean(ThisCountry$Courage) * Age_Ratio_Total$band_70_98[Age_Ratio_Total$Country == country] / 100)
  
  #female, weighted by age
  National_Data$Courage.Mean.Female.Weighted.Age[i] <- 
    mean(ThisCountry$Courage[ThisCountry$Sex == 'female' & ThisCountry$Age <= 18]) * 
    Age_Ratio_Female$band_00_18[Age_Ratio_Female$Country == country] / 100 + 
    mean(ThisCountry$Courage[ThisCountry$Sex == 'female' & ThisCountry$Age >= 19 & ThisCountry$Age <= 24]) * 
    Age_Ratio_Female$band_19_24[Age_Ratio_Female$Country == country] / 100 + 
    mean(ThisCountry$Courage[ThisCountry$Sex == 'female' & ThisCountry$Age >= 25 & ThisCountry$Age <= 49]) * 
    Age_Ratio_Female$band_25_49[Age_Ratio_Female$Country == country] / 100 + 
    ifelse(any(ThisCountry$Sex == 'female' & ThisCountry$Age >= 50 & ThisCountry$Age <= 69), 
           mean(ThisCountry$Courage[ThisCountry$Sex == 'female' & ThisCountry$Age >= 50 & ThisCountry$Age <= 69]) * 
             Age_Ratio_Female$band_50_69[Age_Ratio_Female$Country == country] / 100,
           mean(ThisCountry$Courage[ThisCountry$Sex == 'female']) * 
             Age_Ratio_Female$band_50_69[Age_Ratio_Female$Country == country] / 100) + 
    ifelse(any(ThisCountry$Sex == 'female' & ThisCountry$Age >= 70),
           mean(ThisCountry$Courage[ThisCountry$Sex == 'female' & ThisCountry$Age >= 70]) * 
             Age_Ratio_Female$band_70_98[Age_Ratio_Female$Country == country] / 100,
           mean(ThisCountry$Courage[ThisCountry$Sex == 'female']) * 
             Age_Ratio_Female$band_70_98[Age_Ratio_Female$Country == country] / 100)
  
  #male, weighted by age
  National_Data$Courage.Mean.Male.Weighted.Age[i] <- 
    mean(ThisCountry$Courage[ThisCountry$Sex == 'male' & ThisCountry$Age <= 18]) * 
    Age_Ratio_Male$band_00_18[Age_Ratio_Male$Country == country] / 100 + 
    mean(ThisCountry$Courage[ThisCountry$Sex == 'male' & ThisCountry$Age >= 19 & ThisCountry$Age <= 24]) * 
    Age_Ratio_Male$band_19_24[Age_Ratio_Male$Country == country] / 100 + 
    mean(ThisCountry$Courage[ThisCountry$Sex == 'male' & ThisCountry$Age >= 25 & ThisCountry$Age <= 49]) * 
    Age_Ratio_Male$band_25_49[Age_Ratio_Male$Country == country] / 100 + 
    ifelse(any(ThisCountry$Sex == 'male' & ThisCountry$Age >= 50 & ThisCountry$Age <= 69), 
           mean(ThisCountry$Courage[ThisCountry$Sex == 'male' & ThisCountry$Age >= 50 & ThisCountry$Age <= 69]) * 
             Age_Ratio_Male$band_50_69[Age_Ratio_Male$Country == country] / 100,
           mean(ThisCountry$Courage[ThisCountry$Sex == 'male']) * 
             Age_Ratio_Male$band_50_69[Age_Ratio_Male$Country == country] / 100) + 
    ifelse(any(ThisCountry$Sex == 'male' & ThisCountry$Age >= 70),
           mean(ThisCountry$Courage[ThisCountry$Sex == 'male' & ThisCountry$Age >= 70]) * 
             Age_Ratio_Male$band_70_98[Age_Ratio_Male$Country == country] / 100,
           mean(ThisCountry$Courage[ThisCountry$Sex == 'male']) * 
             Age_Ratio_Male$band_70_98[Age_Ratio_Male$Country == country] / 100)
  
  #weighting by gender and age
  National_Data$Courage.Mean.Weighted.Age.Sex[i] <- 
    National_Data$Courage.Mean.Female.Weighted.Age[i] * (100 / (100 + Sex_Ratio$male_per_100_female[Sex_Ratio$Country == country])) + 
    National_Data$Courage.Mean.Male.Weighted.Age[i] * 
    (Sex_Ratio$male_per_100_female[Sex_Ratio$Country == country] / (100 + Sex_Ratio$male_per_100_female[Sex_Ratio$Country == country]))
  
  #print for progress
  print(paste(i, 'out of 80 countries, Max age =', max(ThisCountry$Age)))
}


##############################
#SAVE FILES
##############################

write.csv(National_Data, "Data/National_Data.csv")
