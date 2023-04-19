#this file is to do the step 1 of our SCA: Generate Specifications

################################
#first, lets have each construct's measures into lists from our data
################################

#courage
Courage_Measures <- c("national_courage")
#the 4 main dependent variables
Individualism_Measures <- c("ipums_idv", "hofstede_idv")
Masculinity_Measures <- c("hofstede_mas")
Terrorism_Measures <- c("global_terrorism_index")
Innovation_Measures <- c("global_innovation_index")
#control variables - we add a "not_controlling" option
Education_Measures <- c(
  "oecd_edu_population_to_primary",
  "oecd_edu_population_to_tertiary",
  "oecd_edu_population_to_secondary",
  "un_edu_primary_complete",
  "un_edu_primary_enroll",
  "un_edu_secondary_enroll",
  "un_edu_tertiary_enroll",
  "wb_edu_literacy_adult",
  "wb_edu_literacy_youth",
  "wb_edu_primary_complete",
  "wb_edu_primary_enroll",
  "wb_edu_secondary_enroll",
  "wb_edu_tertiary_enroll",
  "ipums_edu_tertiary_attain",
  "ipums_edu_adult_literacy",
  "Not_Controlling"
)
Economics_Measures <- c(
  "oecd_gdp", "oecd_gdp_per_cap",
  "un_gdp_per_cap",
  "wb_gdp_per_cap", "wb_gdp", "wb_gdp_per_cap_ppp", "wb_gdp_ppp",
  "Not_Controlling"
)
Urbanisation_Measures <- c(
  "un_urban_ratio",
  "wb_urban_density",
  "wb_urban_largest_city_ratio",
  "wb_urban_big_city_ratio",
  "wb_urban_population_ratio",
  "ipums_urbanisation",
  "Not_Controlling"
)


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

################################
#and save the results
#################################

write.csv(Specs_IDV, "results/Specs_IDV.csv")
write.csv(Specs_INO, "results/Specs_INO.csv")
write.csv(Specs_TER, "results/Specs_TER.csv")
write.csv(Specs_MAS, "results/Specs_MAS.csv")
