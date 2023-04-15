#the purpose of this is to exclude entries in the raw dataset that does not meet our inclusion criteria specifified in the pre-registration

#import main data as csv, which gives N = 2,113,996
Main_Data <- read.csv("TIME_HP_DATA_AUGUST_2021.csv")

#Create a new dataset called TrimedData
#populate it with the rows in MainData that doesn't have NA in column 4 to 34 (all personality questions)
Trimed_Data <- subset(Main_Data, complete.cases(Main_Data[,4:34]) == TRUE) #this gives N = 1,974,332
#filter out those without geo_country info
Trimed_Data <- subset(Trimed_Data, complete.cases(Trimed_Data[,44]) == TRUE) #this gives N = 1,552,399
#filter out those self-reported to be older than 98
Trimed_Data <- subset(Trimed_Data, Trimed_Data[,35] <= 98) #removing these gives N = 1,531,159

#Create a table with the number of eligible responses in each country
Name_Countries <- unique(Trimed_Data$geo_country)
Number_Responses <- c()
for(country in Name_Countries){
  Number_Responses <- c(Number_Responses, sum(Trimed_Data$geo_country == country))
}
Country_Numbers <- data.frame(Name_Countries, Number_Responses)
#filter for countries that have more than 300 responses
Trimed_Countries <- subset(Country_Numbers, Country_Numbers$Number_Responses >= 300) #this gives 80 countries
#now we sum these 80 countries' total responses
sum(Trimed_Countries$Number_Responses) #which gives a final N = 1,523,509

#Finally, create a final dataset with the countries we want to include and export to a csv file for easier future analyses
Final_Data <- subset(Trimed_Data, Trimed_Data$geo_country %in% Trimed_Countries$Name_Countries == TRUE)
write.csv(Final_Data, "Filtered_Raw_Data.csv")



###################################
#additional checks post-analyses: do countries have drastically different age make-ups?
###################################

Raw_Data <- read.csv('Data/Filtered Raw Data.csv')[,-1]

country_list <- unique(Raw_Data$geo_country)[order(unique(Raw_Data$geo_country))]
age_frame <- data.frame(Country = country_list, mean = rep(NA, 80), SE = rep(NA, 80))

#extract mean age and sd for each country
for(row in 1:nrow(age_frame)){
  country <- age_frame$Country[row]
  mean <- mean(Raw_Data$demo_age[Raw_Data$geo_country == country])
  se <- sd(Raw_Data$demo_age[Raw_Data$geo_country == country]) / nrow(Raw_Data[Raw_Data$geo_country == country,])
  age_frame$mean[row] <- mean
  age_frame$SE[row] <- se
  print(row / 80)
}

#upon visual inspection: countries average age range from 18 to 27, but none are above 1SD apart, tho some are multiple SEs apart

