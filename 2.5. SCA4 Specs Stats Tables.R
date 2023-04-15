#this file is to create some summary statistics tables to aid the interpretation of specification results
#Goals are:
#Create summary tables for each specification curve showing for each variable choice:
  #a: number of specifications that has this variable choice
  #b: % of specs with this variable that yielded significant result
  #c: mean effect size of specs with this variable 
  #d: mean Rsq value of specs with this variable

################################
#LIBRARIES AND FILES
################################


library(apaTables)
library(readxl)

Priority <- 1
Specs_IDV <- read.csv(paste("Results/Specs_IDV_P", Priority, ".csv", sep = ''))[,-1]
Specs_INO <- read.csv(paste("Results/Specs_INO_P", Priority, ".csv", sep = ''))[,-1]
Specs_MAS <- read.csv(paste("Results/Specs_MAS_P", Priority, ".csv", sep = ''))[,-1]
Specs_TER <- read.csv(paste("Results/Specs_TER_P", Priority, ".csv", sep = ''))[,-1]

#new specs added on 2022-09-28
Specs_PDI <- read.csv(paste("Results/Raw SCA results/Specs_PDI_P", Priority, ".csv", sep = ''))[,-1]
Specs_UAI <- read.csv(paste("Results/Raw SCA results/Specs_UAI_P", Priority, ".csv", sep = ''))[,-1]
Specs_LTO <- read.csv(paste("Results/Raw SCA results/Specs_LTO_P", Priority, ".csv", sep = ''))[,-1]
Specs_IDG <- read.csv(paste("Results/Raw SCA results/Specs_IDG_P", Priority, ".csv", sep = ''))[,-1]
Specs_TGU <- read.csv(paste("Results/Raw SCA results/Specs_TGU_P", Priority, ".csv", sep = ''))[,-1]
Specs_TGE <- read.csv(paste("Results/Raw SCA results/Specs_TGE_P", Priority, ".csv", sep = ''))[,-1]

National_Data <- read_excel('Data/National_Data.xlsx')
Data_Codebook <- read_excel('Data/National Data Codebook.xlsx')

#################################
#FUNCTION
#################################

#produce SCA summary tables given an SCA results frame and the desired output file name
sca_table <- function(results, file_name){

  #first, extract a list of variable names as our row names
  variables <- c()
  constructs <- c()
  for(col in 1:5){
    variables <- c(variables, unique(results[,col]))
    constructs <- c(constructs, rep(colnames(results)[col], length(unique(results[,col]))))
  }
  
  #make an empty dataframe to populate results
  output <- data.frame(matrix(data = NA, ncol = 6, nrow = length(variables)))
  colnames(output) <- c('Construct', 'Variable Choice', 'n(Specification)', '%Significant', 'Mean Effect', 'Mean R-squard')
  output$Construct <- constructs
  output$`Variable Choice` <- variables
  
  #populate the output dataframe
  for(row in 1:nrow(output)){

    this_spec <- subset(results, results[output$Construct[row]] == output$`Variable Choice`[row])
    
    output$`n(Specification)`[row] <- nrow(this_spec)
    output$`%Significant`[row] <- nrow(subset(this_spec, this_spec$P_Value <= 0.05)) / nrow(this_spec) * 100
    output$`Mean Effect`[row] <- mean(this_spec$Effect_Size)
    output$`Mean R-squard`[row] <- mean(this_spec$R_Squared)
  }
  
  #improve readability
  output$Construct <- gsub('_EDU', ' Education', 
                           gsub('_ECN', ' Economics', gsub('_URB', ' Urbanisation', gsub('x', 'Courage', constructs))))
  output$`Variable Choice` <- gsub('\\.', ' ', 
                                   gsub('_', ' ', 
                                        gsub('X.EDU.', '', gsub('X.GDP', 'GDP', gsub('X.URB.', '', gsub('Courage.Mean.', '', variables))))))
  
  #output and writefile at the same time
  write.csv(output, paste('Results/', file_name, '.csv', sep = ''))
  output
}


###################################
#EXECUTE
###################################

#also splot by age-weighted & age-unweighted
Cour_Age_Unweighted <- c('Courage.Mean.Unweighted', 'Courage.Mean.Male.Unweighted', 'Courage.Mean.Female.Unweighted', 
                         'Courage.Mean.Weighted.Sex')
Cour_Age_Weighted <- c('Courage.Mean.Weighted.Age', 'Courage.Mean.Male.Weighted.Age', 'Courage.Mean.Female.Weighted.Age',
                       'Courage.Mean.Weighted.Age.Sex')

#the unweighted
sca_table(Specs_IDV[Specs_IDV$x %in% Cour_Age_Unweighted,], 'SCA_Table_IDV_Age_Unweighted')
sca_table(Specs_MAS[Specs_MAS$x %in% Cour_Age_Unweighted,], 'SCA_Table_MAS_Age_Unweighted')
sca_table(Specs_TER[Specs_TER$x %in% Cour_Age_Unweighted,], 'SCA_Table_TER_Age_Unweighted')
sca_table(Specs_INO[Specs_INO$x %in% Cour_Age_Unweighted,], 'SCA_Table_INO_Age_Unweighted')
#the weighted ones
sca_table(Specs_IDV[Specs_IDV$x %in% Cour_Age_Weighted,], 'SCA_Table_IDV_Age_Weighted')
sca_table(Specs_MAS[Specs_MAS$x %in% Cour_Age_Weighted,], 'SCA_Table_MAS_Age_Weighted')
sca_table(Specs_TER[Specs_TER$x %in% Cour_Age_Weighted,], 'SCA_Table_TER_Age_Weighted')
sca_table(Specs_INO[Specs_INO$x %in% Cour_Age_Weighted,], 'SCA_Table_INO_Age_Weighted')

#new variables 2022 09 28
sca_table(Specs_PDI[Specs_PDI$x %in% Cour_Age_Unweighted,], 'SCA_Table_PDI_Age_Unweighted')
sca_table(Specs_UAI[Specs_UAI$x %in% Cour_Age_Unweighted,], 'SCA_Table_UAI_Age_Unweighted')
sca_table(Specs_LTO[Specs_LTO$x %in% Cour_Age_Unweighted,], 'SCA_Table_LTO_Age_Unweighted')
sca_table(Specs_IDG[Specs_IDG$x %in% Cour_Age_Unweighted,], 'SCA_Table_IDG_Age_Unweighted')
sca_table(Specs_TGU[Specs_TGU$x %in% Cour_Age_Unweighted,], 'SCA_Table_TGU_Age_Unweighted')
sca_table(Specs_TGE[Specs_TGE$x %in% Cour_Age_Unweighted,], 'SCA_Table_TGE_Age_Unweighted')

sca_table(Specs_PDI[Specs_PDI$x %in% Cour_Age_Weighted,], 'SCA_Table_PDI_Age_Weighted')
sca_table(Specs_UAI[Specs_UAI$x %in% Cour_Age_Weighted,], 'SCA_Table_UAI_Age_Weighted')
sca_table(Specs_LTO[Specs_LTO$x %in% Cour_Age_Weighted,], 'SCA_Table_LTO_Age_Weighted')
sca_table(Specs_IDG[Specs_IDG$x %in% Cour_Age_Weighted,], 'SCA_Table_IDG_Age_Weighted')
sca_table(Specs_TGU[Specs_TGU$x %in% Cour_Age_Weighted,], 'SCA_Table_TGU_Age_Weighted')
sca_table(Specs_TGE[Specs_TGE$x %in% Cour_Age_Weighted,], 'SCA_Table_TGE_Age_Weighted')
