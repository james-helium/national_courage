#this file is to take the specification matrix generated in SCA Step 1 and fill it with regression results
#output of this step will then be visualized in step 3 and statistically analyzed in step 4

#plan: import files, write a generic function for this, execute the function on variables, save the results

#we don't need any additional packages for this


##############################
#we will need our main data file and our specification matrices
##############################

library(readxl)
National_Data <- read_excel("Data/National_Data.xlsx")
#make sure that all are numeric
National_Data[3:52] <- lapply(National_Data[3:52],as.numeric)
#and our specification matrices, removing that first weird index column

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

############################
#first, let's create a function to do this generically to save copying times
###############################

#this function will take in the specification matrix
sca_regression <- function(Specifications){

  #iterate through every row of the specification matrix
  for(row in 1:nrow(Specifications)){

    #first, let's put the currently iterated specification into a data frame
    regression_data <- data.frame(y = National_Data[Specifications$y[row]][,1],
                                  x = National_Data[Specifications$x[row]][,1])
    #for our controls, we will replace the dataset as a string of 0s if it is "Not_Controlling" specification
    if(Specifications$Control_EDU[row] != "Not_Controlling"){
      regression_data <- data.frame(regression_data,
                                    control_education = National_Data[Specifications$Control_EDU[row]][,1])
    } else{
      regression_data <- data.frame(regression_data,
                                    control_education = rep(0, each = 80))}
    
    if(Specifications$Control_ECN[row] != "Not_Controlling"){
      regression_data <- data.frame(regression_data,
                                    control_economics = National_Data[Specifications$Control_ECN[row]][,1])
    } else{
      regression_data <- data.frame(regression_data,
                                    control_economics = rep(0, each = 80))}
    
    if(Specifications$Control_URB[row] != "Not_Controlling"){
      regression_data <- data.frame(regression_data,
                                    control_urbanised = National_Data[Specifications$Control_URB[row]][,1])
    } else{
      regression_data <- data.frame(regression_data,
                                    control_urbanised = rep(0, each = 80))}
    
    #standardizing our data to produce standardized regression coefficient beta, but we don't standardize the 0s
    for(i in 1:ncol(regression_data)){
      if(all(regression_data[,i] == 0) == FALSE){ #only standardize if all of that column is 0 for controlling reasons
        regression_data[,i] <- scale(regression_data[,i])}}
    
    #confirm the colnames for regression
    colnames(regression_data) <- c('y', 'x', 'control_education', 'control_economics', 'control_urbanised')
    
    #not excluding intercept because we have already standardized
    #intercept: we don't remove because we don't have theoretical assumption that it should be 0 - not all variables are perfectly normally distrbuted!
    #also, in our specifications we have included cases of transformations that can reflect correction in our results
    regression_result <- lm(formula = y ~ x + control_education + control_economics + control_urbanised, 
                            data = regression_data)
    regression_result <- summary(regression_result)
    
    
    #now we extract the results of our regression: effect-size as beta, P, SE, R^2, and also N
    #we put them into our specification matrix accordingly
    Specifications$Effect_Size[row] <- regression_result$coefficients['x','Estimate']
    Specifications$P_Value[row] <- regression_result$coefficients['x','Pr(>|t|)']
    Specifications$Standard_Error[row] <- regression_result$coefficients['x','Std. Error']
    Specifications$R_Squared[row] <- regression_result$adj.r.squared
    Specifications$n[row] <- length(regression_result$residuals)
    
    #finally, we will print the row number and progress when we finish in each iteration
    print(row)
  }
  
  #return the specification matrix
  Specifications
}


##############################
#now we use this function on our four variables
#############################

Specs_IDV <- sca_regression(Specs_IDV)
Specs_INO <- sca_regression(Specs_INO)
Specs_MAS <- sca_regression(Specs_MAS)
Specs_TER <- sca_regression(Specs_TER)

Specs_PDI <- sca_regression(Specs_PDI)
Specs_UAI <- sca_regression(Specs_UAI)
Specs_LTO <- sca_regression(Specs_LTO)
Specs_IDG <- sca_regression(Specs_IDG)
Specs_TGU <- sca_regression(Specs_TGU)
Specs_TGE <- sca_regression(Specs_TGE)


############################
#and finally, we save the files
##################################


write.csv(Specs_IDV, paste("Results/Specs_IDV_P", Priority, ".csv", sep = ''))
write.csv(Specs_INO, paste("Results/Specs_INO_P", Priority, ".csv", sep = ''))
write.csv(Specs_TER, paste("Results/Specs_TER_P", Priority, ".csv", sep = ''))
write.csv(Specs_MAS, paste("Results/Specs_MAS_P", Priority, ".csv", sep = ''))

write.csv(Specs_PDI, paste("Results/Raw SCA results/Specs_PDI_P", Priority, ".csv", sep = ''))
write.csv(Specs_UAI, paste("Results/Raw SCA results/Specs_UAI_P", Priority, ".csv", sep = ''))
write.csv(Specs_LTO, paste("Results/Raw SCA results/Specs_LTO_P", Priority, ".csv", sep = ''))
write.csv(Specs_IDG, paste("Results/Raw SCA results/Specs_IDG_P", Priority, ".csv", sep = ''))
write.csv(Specs_TGU, paste("Results/Raw SCA results/Specs_TGU_P", Priority, ".csv", sep = ''))
write.csv(Specs_TGE, paste("Results/Raw SCA results/Specs_TGE_P", Priority, ".csv", sep = ''))

