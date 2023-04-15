#this file is to do the step 1 of our SCA: Generate Specifications
#we aim to output a matrix of specifications to be then processed by another file to fill-in regression results

#plan: import data, extract lists of measures for each constructs, write a function to turn constructs into specifications, 
#run the function on our key variables, save the specification matrices


#first, we need packages
library(readxl)


#####################
#import relevant data file
##########################

Data_Codebook <- read_xlsx("Data/National Data Codebook.xlsx")


################################
#first, lets have each construct's measures into lists from our code book
################################


#set the priority limit, if 3, then 1-3 will all be counted in
Priority <- 1


#courage
Courage_Measures <- subset(Data_Codebook$`Record Code`, Data_Codebook$Construct == "Courage" & 
                             Data_Codebook$Priority <= Priority)
#the 4 main dependent variables
Individualism_Measures <- subset(Data_Codebook$`Record Code`, Data_Codebook$Construct == "Individualism" &
                                   Data_Codebook$Priority <= Priority)
Masculinity_Measures <- subset(Data_Codebook$`Record Code`, Data_Codebook$Construct == "Masculinity" &
                                 Data_Codebook$Priority <= Priority)
Terrorism_Measures <- subset(Data_Codebook$`Record Code`, Data_Codebook$Construct == "Terrorism" &
                               Data_Codebook$Priority <= Priority)
Innovation_Measures <- subset(Data_Codebook$`Record Code`, Data_Codebook$Construct == "Innovation" &
                                Data_Codebook$Priority <= Priority)
#control variables - we add a "Not_Controlling" option to help with enumerating all possible specifications
Education_Measures <- c(subset(Data_Codebook$`Record Code`, Data_Codebook$Construct == "Education" &
                                 Data_Codebook$Priority <= Priority),
                        "Not_Controlling")
Economics_Measures <- c(subset(Data_Codebook$`Record Code`, Data_Codebook$Construct == "Economics" &
                                 Data_Codebook$Priority <= Priority),
                        "Not_Controlling")
Urbanisation_Measures <- c(subset(Data_Codebook$`Record Code`, Data_Codebook$Construct == "Urbanisation" &
                                    Data_Codebook$Priority <= Priority),
                           "Not_Controlling")


#####################################
#create a function for generate specification matrix
#####################################

#this function takes in what we want as y and x measures, and output a specification matrix with NAs for results
sca_specifications <- function(x_measures, y_measures){
  
  #first, let's calculate the number of specifications we have, by multiplying to get all possible combinations of choices;
  #choices: which measure of x & y, which choice of control measures, if we are controlling at all 
  Num_of_Specs <- length(y_measures) * length(x_measures) *
    length(Education_Measures) * length(Economics_Measures) * length(Urbanisation_Measures)
  
  #then, create a (huge) matrix for all our specifications
  Specifications <- data.frame(matrix(data = NA, nrow = Num_of_Specs, ncol = 10))
  #we will always have our dependent variable first, so make life easier
  colnames(Specifications) <- c("y", "x", "Control_EDU", "Control_ECN", "Control_URB",
                                "Effect_Size", "P_Value", "Standard_Error", "R_Squared", "n")
  
  #and populate this matrix with our specifications, leaving the results sections as NA
  #for each y measure, repeat n times to fill the entire column (abc -> aaabbbccc)
  Specifications$y <- rep(y_measures, each = nrow(Specifications) / length(y_measures))
  #for each x measure, repeat n times to fill the same x (each), then do the same for all number of x (times)
  Specifications$x <- rep(rep(x_measures,
                              #this here is to repeat y for n times to fill the same x
                              each = nrow(Specifications) / (length(y_measures) * length(x_measures))), 
                          #then repeat what we did for the number of x
                          times = length(y_measures))
  #now we keep doing this for the next four control specifications... same ideas
  Specifications$Control_EDU <- 
    rep(rep(rep(Education_Measures,
                each = nrow(Specifications) /
                  (length(y_measures) * length(x_measures) * length(Education_Measures))), 
            times = length(y_measures)),
        times = length(x_measures))
  #for control specifications of economic controls
  Specifications$Control_ECN <- 
    rep(rep(rep(rep(Economics_Measures,
                    each = nrow(Specifications) /
                      (length(y_measures) * length(x_measures) * length(Education_Measures) * length(Economics_Measures))), 
                times = length(y_measures)),
            times = length(x_measures)),
        times = length(Education_Measures))
  #for specifications of urbanisation controls
  Specifications$Control_URB <-
    rep(rep(rep(rep(rep(Urbanisation_Measures,
                        each = nrow(Specifications) /
                          (length(y_measures) * length(x_measures) * length(Education_Measures) * length(Economics_Measures) * length(Urbanisation_Measures))), 
                    times = length(y_measures)),
                times = length(x_measures)),
            times = length(Education_Measures)),
        times = length(Economics_Measures))
  
  #finally, we return the specification matrix
  Specifications
}


#############################################
#now we perform this function on all four of our variables
#############################################

Specs_IDV <- sca_specifications(x_measures = Courage_Measures, y_measures = Individualism_Measures)
Specs_MAS <- sca_specifications(x_measures = Courage_Measures, y_measures = Masculinity_Measures)
Specs_TER <- sca_specifications(x_measures = Courage_Measures, y_measures = Terrorism_Measures)
Specs_INO <- sca_specifications(x_measures = Courage_Measures, y_measures = Innovation_Measures)

#additional dependent measures added on 2022-09-28
Specs_PDI <- sca_specifications(x_measures = Courage_Measures, y_measures = 'PowerDiss')
Specs_UAI <- sca_specifications(x_measures = Courage_Measures, y_measures = 'UncertAvoid')
Specs_LTO <- sca_specifications(x_measures = Courage_Measures, y_measures = 'LongTermO')
Specs_IDG <- sca_specifications(x_measures = Courage_Measures, y_measures = 'Indulgence')
Specs_TGU <- sca_specifications(x_measures = Courage_Measures, y_measures = 'Tightness.Uz')
Specs_TGE <- sca_specifications(x_measures = Courage_Measures, y_measures = 'Tightness.Gelfand')

################################
#and save the results
#################################

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






