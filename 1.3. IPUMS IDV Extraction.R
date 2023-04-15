#This file is to extract Individualism from IPUMS-I data
#following Santos, Warnum & Grossmann, 2017 # nolint


#################################
#FILES AND LIBRARIES
################################

install.packages("ipumsr")
install.packages("labelled")

library(ipumsr)
library(labelled)

ddi <- read_ipums_ddi("data/ipumsi_00002.xml")
IPUMS_Data <- read_ipums_micro(ddi)

#################################
#IPUMS EXTRACTION
##################################

#extracting:
#average household size: number of related persons per household
#percentage of lone-person households: overal and for 60+s
#ratio of divorce/sep and marriage in each country

IPUMS_H <- subset(IPUMS_Data, IPUMS_Data$RECTYPE == 'H') #that's 5 million households!
IPUMS_P <- subset(IPUMS_Data, IPUMS_Data$RECTYPE == 'P') #14.4 million individuals...
#remove households with no-one in them
IPUMS_H <- subset(IPUMS_H, IPUMS_H$PERSONS != 0) #still 5.45 mil left

#create a dataframe to collect extraction results
IPUMS_IDV <- data.frame(matrix(data = NA, nrow = 50, ncol = 5))
colnames(IPUMS_IDV) <- c('Country', 'related.household.size', 'percent.lone.household', 'percent.lone.old', 'divorce.ratio')
IPUMS_IDV$Country <- unique(IPUMS_Data$COUNTRY) #50 countries on our list

#extraction
for(i in 1:nrow(IPUMS_IDV)){
  country <- IPUMS_IDV$Country[i]
  print(paste('Country', i))
  
  this_country_h <- subset(IPUMS_H, IPUMS_H$COUNTRY == country)
  this_country_p <- subset(IPUMS_P, IPUMS_P$COUNTRY == country)
  print(paste(nrow(this_country_h), 'households and', nrow(this_country_p), 'people'))
  
  people_over_60 <- subset(this_country_p$SERIAL, this_country_p$AGE >= 60)
  IPUMS_IDV$related.household.size[i] <- mean(this_country_h$PERSONS - this_country_h$UNREL)
  IPUMS_IDV$percent.lone.household[i] <- nrow(subset(this_country_h, this_country_h$PERSONS == 1)) / nrow(this_country_h)
  IPUMS_IDV$percent.lone.old[i] <- nrow(subset(this_country_h, 
                                                 this_country_h$PERSONS == 1 
                                                 & this_country_h$SERIAL %in% people_over_60)) / nrow(this_country_h)
  IPUMS_IDV$divorce.ratio[i] <- nrow(subset(this_country_p, 
                                                this_country_p$MARST == 3)) / nrow(subset(this_country_p, 
                                                                                          this_country_p$MARST == 2 | this_country_p$MARST == 4))
  
  print(paste('household size =', IPUMS_IDV$related.household.size[i]))
  print(paste('lone household =', IPUMS_IDV$percent.lone.household[i]))
  print(paste('lone old house =', IPUMS_IDV$percent.lone.old[i]))
  print(paste('divorces ratio =', IPUMS_IDV$divorce.ratio[i]))
}

#replace country code by their real name so that we can read it
country_labels <- data.frame(val_labels(IPUMS_Data$COUNTRY))
country_labels <- data.frame(country_labels, labels(country_labels))[,-3]
colnames(country_labels) <- c('code', 'name')
IPUMS_IDV$Country <- as.integer(IPUMS_IDV$Country)
for(i in 1:nrow(IPUMS_IDV)){
  IPUMS_IDV$Country[i] <- subset(country_labels$name, country_labels$code == IPUMS_IDV$Country[i])
}

#some countries have null or faulty results, we replace them by global mean
mean_hsh_size <- mean(IPUMS_IDV$related.household.size, na.rm = T)
mean_lone_hsh <- mean(IPUMS_IDV$percent.lone.household, na.rm = T)
mean_div_rate <- mean(IPUMS_IDV$divorce.ratio, na.rm = T)
for(i in 1:nrow(IPUMS_IDV)){
  if(is.na(IPUMS_IDV$related.household.size[i])){
    IPUMS_IDV$related.household.size[i] <- mean_hsh_size
  }
  if(IPUMS_IDV$percent.lone.household[i] == 1){
    IPUMS_IDV$percent.lone.household[i] <- mean_lone_hsh
  }
  if(is.na(IPUMS_IDV$divorce.ratio[i])){
    IPUMS_IDV$divorce.ratio[i] <- mean_div_rate
  }
}
#standardise them
IPUMS_IDV[,-1] <- scale(IPUMS_IDV[,-1])
scores <- c()
for(i in 1:nrow(IPUMS_IDV)){
  scores <- c(scores, mean(-IPUMS_IDV$related.household.size[i],
                           IPUMS_IDV$percent.lone.household[i],
                           IPUMS_IDV$percent.lone.old[i],
                           IPUMS_IDV$divorce.ratio[i]))
}
IPUMS_IDV <- data.frame(Country = IPUMS_IDV$Country,
                        IPUMS_IDV = scores)
IPUMS_IDV$Country[40] <- 'Slovakia'

############################
#SAVE TO FILE
############################

write.csv(IPUMS_IDV, 'data/ipums_idv_extract.csv')
