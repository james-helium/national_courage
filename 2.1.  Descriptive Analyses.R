#this file is to produce initial descriptive analyses:
#1) world map 
#2) descriptive stats tables

###############################
#LIBRARIES AND DATA
###############################

library(ggplot2)
library(apaTables)
library(readxl)
library(rnaturalearth)
library(rnaturalearthdata)

NationalData <- read_excel("Data/National_Data.xlsx")
Country_Regions <- read.csv("External Data/Country_by_Region.csv")


###############################
#PLOT WORLD MAPS
###############################

#create a simple dataframe object "WorldCoutnries" that contains names of countries and their location etc. for ggplot to plot out
WorldCountries <- ne_countries(scale = "medium", returnclass = "sf")
#removing "antarctica" because it's quite irrelevant
WorldCountries <- subset(WorldCountries, WorldCountries$sovereignt != "Antarctica")
#changing names to match across dataframes
WorldCountries$admin[226] <- "United States"
WorldCountries$admin[90] <- "Hong Kong"
WorldCountries$admin[56] <- "Czechia"
WorldCountries$admin[34] <- "Brunei Darussalam"
WorldCountries$admin[200] <- "Serbia"

#write a function to plot since there are so many different versions of courage
plot_world_map <- function(Courage_Version = 'Courage.Mean.Unweighted', label = ''){
  #create courage data list for plot
  Courage <- c()
  #look over all the territories in the world countries dataset, and see if they are in our dataset
  for(territories in WorldCountries$admin){
    #if they are in our dataset, add the data from our dataset into the empty list in the right order
    ifelse(territories %in% NationalData$Country.Name,
           Courage <- c(Courage, NationalData[NationalData$Country.Name == territories, Courage_Version]),
           #if not, fill in NA
           Courage <- c(Courage, NA))
  }
  
  #now, transform the scores into z-score around mean, using the function scale()
  Courage_Z <- scale(Courage)[,1]
  
  #now plot - let's see the z score of mean courage
  plot <- ggplot(data = WorldCountries) + 
    geom_sf(aes(fill = Courage_Z), color = "darkgrey") +
    scale_fill_gradient2(low = "green", mid = "white", high = "red", midpoint = 0, na.value = "grey", name = "") +
    ggtitle(label, subtitle = "z-score around global mean")
}

#plot them 
Plot_Mean_Unweighted <- plot_world_map(Courage_Version = 'Courage.Mean.Unweighted', 
                                       label = 'National Average Courage (Unweighted)')
Plot_Mean_Weighted_Age <- plot_world_map(Courage_Version = 'Courage.Mean.Weighted.Age', 
                                         label = 'National Average Courage (Age-Weighted)')
Plot_Mean_Weighted_Sex <- plot_world_map(Courage_Version = 'Courage.Mean.Weighted.Sex', 
                                         label = 'National Average Courage (Sex-Weighted)')
Plot_Mean_Male_Unweighted <- plot_world_map(Courage_Version = 'Courage.Mean.Male.Unweighted', 
                                            label = 'National Average Male Courage (Unweighted)')
Plot_Mean_Female_Unweighted <- plot_world_map(Courage_Version = 'Courage.Mean.Female.Unweighted', 
                                              label = 'National Average Female Courage (Unweighted)')
Plot_Mean_Male_Weighted <- plot_world_map(Courage_Version = 'Courage.Mean.Male.Weighted.Age', 
                                            label = 'National Average Male Courage (Age-Weighted)')
Plot_Mean_Female_Weighted <- plot_world_map(Courage_Version = 'Courage.Mean.Female.Weighted.Age', 
                                              label = 'National Average Female Courage (Age-Weighted)')
Plot_Mean_Weighted_Age_Sex <- plot_world_map(Courage_Version = 'Courage.Mean.Weighted.Age.Sex', 
                                            label = 'National Average Courage (Age-and-Sex-Weighted)')

###############################
#DESCRIPTIVE STATS TABLES
###############################

#correlation table of the courage measures
Courage_Measures <- data.frame(Courage.Mean.Unweighted = NationalData$Courage.Mean.Unweighted,
                               Courage.Mean.Female.Unweighted = NationalData$Courage.Mean.Female.Unweighted,
                               Courage.Mean.Male.Unweighted = NationalData$Courage.Mean.Male.Unweighted,
                               Courage.Mean.Weighted.Age = NationalData$Courage.Mean.Weighted.Age,
                               Courage.Mean.Weighted.Sex = NationalData$Courage.Mean.Weighted.Sex,
                               Courage.Mean.Weighted.Age.Sex = NationalData$Courage.Mean.Weighted.Age.Sex,
                               Courage.Mean.Female.Weighted.Age = NationalData$Courage.Mean.Female.Weighted.Age,
                               Courage.Mean.Male.Weighted.Age = NationalData$Courage.Mean.Male.Weighted.Age)
apa.cor.table(data = Courage_Measures, filename = 'Results/Correlation_Courage.rtf')

#correlation table of major courage measures and IDV measures
IDV_Courage_Measures <- data.frame(Courage.Mean.Unweighted = NationalData$Courage.Mean.Unweighted,
                                   Courage.Mean.Weighted.Age.Sex = NationalData$Courage.Mean.Weighted.Age.Sex,
                                   Courage.Mean.Female.Weighted.Age = NationalData$Courage.Mean.Female.Weighted.Age,
                                   Courage.Mean.Male.Weighted.Age = NationalData$Courage.Mean.Male.Weighted.Age,
                                   IDV.Hof = NationalData$IDV.Hofstede,
                                   IDV.IPU = NationalData$IDV.IPUMS)
apa.cor.table(data = IDV_Courage_Measures, filename = 'Results/Correlation_IDV.rtf')

#correlation table of major courage measures and MAS Measure
MAS_Courage_Measures <- data.frame(Courage.Mean.Unweighted = NationalData$Courage.Mean.Unweighted,
                                   Courage.Mean.Weighted.Age.Sex = NationalData$Courage.Mean.Weighted.Age.Sex,
                                   Courage.Mean.Female.Weighted.Age = NationalData$Courage.Mean.Female.Weighted.Age,
                                   Courage.Mean.Male.Weighted.Age = NationalData$Courage.Mean.Male.Weighted.Age,
                                   MAS = NationalData$MAS.Hofstede)
apa.cor.table(data = MAS_Courage_Measures, filename = 'Results/Correlation_MAS.rtf')

#correlation table of major courage measures and TER measures
TER_Courage_Measures <- data.frame(Courage.Mean.Unweighted = NationalData$Courage.Mean.Unweighted,
                                   Courage.Mean.Weighted.Age.Sex = NationalData$Courage.Mean.Weighted.Age.Sex,
                                   Courage.Mean.Female.Weighted.Age = NationalData$Courage.Mean.Female.Weighted.Age,
                                   Courage.Mean.Male.Weighted.Age = NationalData$Courage.Mean.Male.Weighted.Age,
                                   TER = NationalData$TER.GTI.18_20)
apa.cor.table(data = TER_Courage_Measures, filename = 'Results/Correlation_TER.rtf')

#correlation table of major courage measures and INO measures
INO_Courage_Measures <- data.frame(Courage.Mean.Unweighted = NationalData$Courage.Mean.Unweighted,
                                   Courage.Mean.Weighted.Age.Sex = NationalData$Courage.Mean.Weighted.Age.Sex,
                                   Courage.Mean.Female.Weighted.Age = NationalData$Courage.Mean.Female.Weighted.Age,
                                   Courage.Mean.Male.Weighted.Age = NationalData$Courage.Mean.Male.Weighted.Age,
                                   INO.GII = NationalData$INO.GII.17_21,
                                   INO.III = NationalData$INO.III2009,
                                   INO.Creativity = NationalData$INO.CreativityIndex)
apa.cor.table(data = INO_Courage_Measures, filename = 'Results/Correlation_INO.rtf')

#correlation table of major courage measures and major control variables
CONTROL_Courage_Measures <- data.frame(Courage.Mean.Unweighted = NationalData$Courage.Mean.Unweighted,
                                   Courage.Mean.Weighted.Age.Sex = NationalData$Courage.Mean.Weighted.Age.Sex,
                                   Courage.Mean.Female.Weighted.Age = NationalData$Courage.Mean.Female.Weighted.Age,
                                   Courage.Mean.Male.Weighted.Age = NationalData$Courage.Mean.Male.Weighted.Age,
                                   Control.GDP = NationalData$X.GDP.percap.UN.16_20,
                                   Control.Edu.Lit = NationalData$X.EDU.IPUMS.Literacy,
                                   Control.Edu.Ter = NationalData$X.EDU.UN.Ter.Enrl.Rate.18,
                                   Control.Urb = NationalData$X.URB.UN.Urban.Pop.17_20)
apa.cor.table(data = CONTROL_Courage_Measures, filename = 'Results/Correlation_Controls.rtf')

#2022-09-28 additional correlations on other hofstede measures and tightness with the main courage measures
ADDITIONAL_Cour_Corr <- data.frame(Courage.Unweighted = NationalData$Courage.Mean.Unweighted,
                                   Courage.Weighted.Age.Sex = NationalData$Courage.Mean.Weighted.Age.Sex,
                                   Tightness.Uz = NationalData$Tightness.Uz,
                                   Tightness.Eriksson = NationalData$Tightness.Gelfand,
                                   Hofstede.Power.Distance = NationalData$PowerDiss,
                                   Hofstede.Long.Tm.Orient = NationalData$LongTermO,
                                   Hofstede.Indulgence = NationalData$Indulgence,
                                   Hofstede.Uncertan.Avoid = NationalData$UncertAvoid)
ADDITIONAL_Cour_Corr <- data.frame(lapply(ADDITIONAL_Cour_Corr, as.numeric))
apa.cor.table(data = ADDITIONAL_Cour_Corr, filename = 'Results/Correlation_Additional_Dep.rtf')

###############################
#EXPORT
###############################

#the correlation tables are already exported, so just the worldmaps here
pdf(file = 'Results/World Map Plots.pdf', width = 16, height = 7)
Plot_Mean_Unweighted
Plot_Mean_Weighted_Age
Plot_Mean_Weighted_Sex
Plot_Mean_Weighted_Age_Sex
Plot_Mean_Male_Unweighted
Plot_Mean_Female_Unweighted
Plot_Mean_Male_Weighted
Plot_Mean_Female_Weighted
dev.off()
