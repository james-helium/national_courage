#This file is to take WVS and IPUMS-I data into the main National Dataset
#goal: extract individualism information from WVS and IPUMS following Santos, Warnum & Grossmann, 2017


#################################
#FILES AND LIBRARIES
################################

library(ipumsr)
library(labelled)

WVS_Raw_Data <- read.csv('External Data/WVS/WVS7.csv')
National_Data <- read.csv('Data/National_Data.csv')[,-1]
ddi <- read_ipums_ddi("External Data/IPUMS-I/ipumsi_00002.xml")
IPUMS_Data <- read_ipums_micro(ddi)


#################################
#WVS EXTRACTION
##################################

#filter the WVS data to keep only the questions we want
#they are Q# 1, 2, 8, 48, 154, 155
WVS_Filtered <- data.frame(Country = WVS_Raw_Data$B_COUNTRY_ALPHA,
                           Q1 = WVS_Raw_Data$Q1,
                           Q2 = WVS_Raw_Data$Q2,
                           Q8 = WVS_Raw_Data$Q8,
                           Q48 = WVS_Raw_Data$Q48,
                           Q154 = WVS_Raw_Data$Q154,
                           Q155 = WVS_Raw_Data$Q155)
#make sure that all the values are positive
WVS_Filtered[,-1] <- abs(WVS_Filtered[,-1])

#now, summarize each person's answers into their personal individualism score following SW&G2017
friends_over_family <- WVS_Filtered$Q2 - WVS_Filtered$Q1
independent_children <- 2 - WVS_Filtered$Q8
pref_for_expression <- ifelse(WVS_Filtered$Q154 == 2 | WVS_Filtered$Q154 == 4, 1, 0) +
  ifelse(WVS_Filtered$Q155 == 2 | WVS_Filtered$Q155 == 4, 1, 0)
#substitude these into the filtered frame instead of the Q numbers
WVS_Filtered <- data.frame(Country = WVS_Filtered$Country,
                           friends_over_family, independent_children, pref_for_expression,
                           #we add a fourth indicator 'choice_in_life' from Q48
                           choice_in_life = WVS_Filtered$Q48)

#then we summarize these four indicators into country level scores weighing percentage over thresholds & average value
#we will combine these indicators together after standardising them across all countries
countries <- unique(WVS_Filtered$Country)
fof_list <- c()
idc_list <- c()
pfe_list <- c()
cil_list <- c()
for(i in 1:length(countries)){
  country_i <- subset(WVS_Filtered, WVS_Filtered$Country == countries[i])
  n_i <- nrow(country_i)
  fof_list <- c(fof_list, nrow(subset(country_i, country_i$friends_over_family > 0)) / n_i * mean(country_i$friends_over_family))
  idc_list <- c(idc_list, nrow(subset(country_i, country_i$independent_children == 1)) / n_i * mean(country_i$independent_children))
  pfe_list <- c(pfe_list, nrow(subset(country_i, country_i$pref_for_expression > 0)) / n_i * mean(country_i$pref_for_expression))
  cil_list <- c(cil_list, nrow(subset(country_i, country_i$choice_in_life > 5)) / n_i * mean(country_i$choice_in_life))
  print(paste(i, countries[i], 'N =', n_i, 'mean scores =', mean(c(fof_list[i] + idc_list[i] + pfe_list[i] + cil_list[i]))))
}
#combine into data frame
WVS_IDV <- data.frame(Country = countries, fof_list, idc_list, pfe_list, cil_list)
#standardise all into normal z score
WVS_IDV[,-1] <- scale(WVS_IDV[,-1])
#now we average the z scores to produce our final national individualism score from WVS
national_idv <- c()
for(i in 1:nrow(WVS_IDV)){
  score <- mean(as.numeric(WVS_IDV[i, -1]))
  national_idv <- c(national_idv, score)
  print(paste(i, 'score =', score))
}
WVS_IDV <- data.frame(Country = WVS_IDV$Country, WVS_IDV = national_idv)
#we manually add this into the National Data file because country names are all messed up and incompatible


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


############################
#SAVE TO FILE
############################

IPUMS_IDV$Country[40] <- 'Slovakia'
national_list <- c()
for(i in 1:nrow(National_Data)){
  if(National_Data$Country[i] %in% IPUMS_IDV$Country){
    val <- subset(IPUMS_IDV$IPUMS_IDV, IPUMS_IDV$Country == National_Data$Country[i])
  } else{
    val <- NA
  }
  national_list <- c(national_list, val)
  print(paste(i, National_Data$Country[i], val))
}
National_Data <- data.frame(National_Data, IDV.IPUMS = national_list)
write.csv(National_Data, 'Data/National_Data.csv')

