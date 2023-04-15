#This file is to take individualism from WVS
#following Santos, Warnum & Grossmann, 2017 # nolint


#################################
#FILES AND LIBRARIES
################################

wvs_7 <- read.csv("data/wvs_7.csv")

#################################
#WVS EXTRACTION
##################################

#make sure that all the values are positive
wvs_7[, -1] <- abs(wvs_7[, -1])

#summarize each person's answers into their personal individualism score
friends_over_family <- wvs_7$Q2 - wvs_7$Q1
independent_children <- 2 - wvs_7$Q8
pref_for_expression <- ifelse(wvs_7$Q154 == 2 | wvs_7$Q154 == 4, 1, 0) +
  ifelse(wvs_7$Q155 == 2 | wvs_7$Q155 == 4, 1, 0)
#substitude these into the filtered frame instead of the Q numbers
wvs_7 <- data.frame(
  Country = wvs_7$B_COUNTRY_ALPHA,
  friends_over_family,
  independent_children,
  pref_for_expression,
  #we add a fourth indicator 'choice_in_life' from Q48
  choice_in_life = wvs_7$Q48
)

#then we summarize these four indicators into country level scores
#weighing percentage over thresholds & average value
#we will combine these indicators together after standardising them
countries <- unique(wvs_7$Country)
fof_list <- c()
idc_list <- c()
pfe_list <- c()
cil_list <- c()
for (i in 1:length(countries)){
  country_i <- subset(wvs_7, wvs_7$Country == countries[i])
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

#save the file
write.csv(WVS_IDV, 'data/wvs_idv_extract.csv')
