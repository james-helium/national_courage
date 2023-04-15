#This file is to extract control variables from IPUMS
#because the raw file size is too large to backup on github


##################################
#LIBRARIES AND FILES
##################################

IPUMS_Edu_Urb <- read.csv('data/ipumsi_controls_data.csv')

#################################
#STANDARDIZING COUNTRY NAMES
#################################

#IPUMS country coding is through numbers and they didn't include labels in the csv, so we get it from script 1.3 "country_labels"
IPUMS_Data <- read_ipums_micro(read_ipums_ddi("data/ipumsi_00002.xml")) #file not use elsewhere in this script
country_labels <- data.frame(val_labels(IPUMS_Data$COUNTRY)) #get labels 
rm(IPUMS_Data) #since we don't need it anymore
country_labels <- data.frame(country_labels, labels(country_labels))[,-3]
colnames(country_labels) <- c('code', 'name')
#make the labels standardised to our country names
country_labels$name[country_labels$name == 'Slovak Republic'] <- 'Slovakia'
#replace the country coding to actual country names takes way too long, so we do it only after extraction.
#here, we write a function to help with later
country_name_ipums <- function(frame){
  frame$Country <- as.integer(frame$Country)
  for(i in 1:nrow(frame)){
    frame$Country[i] <- country_labels$name[country_labels$code == frame$Country[i]]
    print(paste(i / nrow(frame) * 100))
  }
  frame
}


##################################
#EXTRACTION
##################################

#IPUMS - we just extract the percentage of uni completed as marked in education attainment (val == 4) and literacy (val == 2)
IPUMS_countries <- unique(IPUMS_Edu_Urb$COUNTRY)
X.EDU.IPUMS.Ter.Attain.Rate <- c()
X.EDU.IPUMS.Literacy <- c()
for(i in 1:length(IPUMS_countries)){
  country <- IPUMS_countries[i]
  this_country <- IPUMS_Edu_Urb[IPUMS_Edu_Urb$COUNTRY == country,]
  ter.attain.rate <- nrow(this_country[this_country$EDATTAIN == 4,]) / nrow(this_country)
  literacy <- nrow(this_country[this_country$LIT == 2,]) / nrow(this_country)
  X.EDU.IPUMS.Ter.Attain.Rate <- c(X.EDU.IPUMS.Ter.Attain.Rate, ter.attain.rate)
  X.EDU.IPUMS.Literacy <- c(X.EDU.IPUMS.Literacy, literacy)
  print(paste(i, 'out of', length(IPUMS_countries), 'literacy =', literacy))
}
X.EDU.IPUMS <- data.frame(Country = IPUMS_countries, X.EDU.IPUMS.Ter.Attain.Rate, X.EDU.IPUMS.Literacy)
#remember to change the country names to our standardised names
X.EDU.IPUMS <- country_name_ipums(X.EDU.IPUMS)


#IPUMS - find the percentage of those in urban in each country (val == 2)
X.URB.IPUMS <- c()
for(i in 1:length(IPUMS_countries)){
  country <- IPUMS_countries[i]
  this_country <- IPUMS_Edu_Urb[IPUMS_Edu_Urb$COUNTRY == country,]
  urban.rate <- nrow(this_country[this_country$URBAN == 2,]) / nrow(this_country)
  X.URB.IPUMS <- c(X.URB.IPUMS, urban.rate)
  print(paste(i, 'out of', length(IPUMS_countries), 'urbanisation =', urban.rate))
}
X.URB.IPUMS <- data.frame(Country = IPUMS_countries, X.URB.IPUMS)
X.URB.IPUMS <- country_name_ipums(X.URB.IPUMS)


####################################
#COMBINE TO ONE DATA FRAME
###################################

output <- merge(X.EDU.IPUMS, X.URB.IPUMS, by = 'Country')

####################################
#SAVE FILES
###################################

write.csv(output, "data/ipums_controls_extract.csv")
