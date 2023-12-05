# this file is to do the step 1 of our SCA: Generate Specifications

################################
# first, lets have each construct's measures into lists from our data
################################

# courage
Courage_Measures <- c("national_courage")
# the 7 cultural dependent variables
Individualism_Measures <- c("ipums_idv", "hofstede_idv")
Masculinity_Measures <- c("hofstede_mas")
PowerDistance_Measures <- c("hofstede_pwd")
UncertaintyAvoidance_Measures <- c("hofstede_una")
LongtermOrient_Measures <- c("hofstede_lto")
Indulgence_Measures <- c("hofstede_idg")
Tightness_Measures <- c("uz_tightloose", "eriksson_tightloose")
# the 2 outcome variables
Terrorism_Measures <- c("global_terrorism_index")
Innovation_Measures <- c("global_innovation_index")
# control variables - we add a "not_controlling" option
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

# additional exploratory dvs
big_five_openness <- c("allik_big_five_openness")
big_five_conscientiousness <- c("allik_big_five_conscientiousness")
big_five_extraversion <- c("allik_big_five_extraversion")
big_five_agreeableness <- c("allik_big_five_agreeableness")
big_five_neuroticism <- c("allik_big_five_neuroticism")
global_giving_index_rank <- c("global_giving_index_rank")
future_orientation <- c(
  "GLOBE_future_orientation_societal_practices",
  "GLOBE_future_orientation_societal_values"
)
humane_orientation <- c(
  "GLOBE_humane_orientation_societal_practices",
  "GLOBE_humane_orientation_societal_values"
)
performance_orientation <- c(
  "GLOBE_performance_orientation_societal_practices",
  "GLOBE_performance_orientation_societal_values"
)
rosenberg_self_esteem <- c("rosenberg_self_esteem")



#####################################
# create a function for generate specification matrix
#####################################

# this function takes in what we want as y and x measures, and output a specification matrix with NAs for results
sca_specifications <- function(x_measures, y_measures) {

  # first, let's calculate the number of specifications we have, by multiplying to get all possible combinations of choices;
  # choices: which measure of x & y, which choice of control measures, if we are controlling at all
  Num_of_Specs <- length(y_measures) * length(x_measures) *
    length(Education_Measures) * length(Economics_Measures) * length(Urbanisation_Measures)

  # then, create a (huge) matrix for all our specifications
  Specifications <- data.frame(matrix(data = NA, nrow = Num_of_Specs, ncol = 10))
  # we will always have our dependent variable first, so make life easier
  colnames(Specifications) <- c(
    "y", "x", "Control_EDU", "Control_ECN", "Control_URB",
    "Effect_Size", "P_Value", "Standard_Error", "R_Squared", "n"
  )

  # and populate this matrix with our specifications, leaving the results sections as NA
  # for each y measure, repeat n times to fill the entire column (abc -> aaabbbccc)
  Specifications$y <- rep(y_measures, each = nrow(Specifications) / length(y_measures))
  # for each x measure, repeat n times to fill the same x (each), then do the same for all number of x (times)
  Specifications$x <- rep(rep(x_measures,
    # this here is to repeat y for n times to fill the same x
    each = nrow(Specifications) / (length(y_measures) * length(x_measures))
  ),
  # then repeat what we did for the number of x
  times = length(y_measures)
  )
  # now we keep doing this for the next four control specifications... same ideas
  Specifications$Control_EDU <-
    rep(rep(rep(Education_Measures,
      each = nrow(Specifications) /
        (length(y_measures) * length(x_measures) * length(Education_Measures))
    ),
    times = length(y_measures)
    ),
    times = length(x_measures)
    )
  # for control specifications of economic controls
  Specifications$Control_ECN <-
    rep(rep(rep(rep(Economics_Measures,
      each = nrow(Specifications) /
        (length(y_measures) * length(x_measures) * length(Education_Measures) * length(Economics_Measures))
    ),
    times = length(y_measures)
    ),
    times = length(x_measures)
    ),
    times = length(Education_Measures)
    )
  # for specifications of urbanisation controls
  Specifications$Control_URB <-
    rep(rep(rep(rep(rep(Urbanisation_Measures,
      each = nrow(Specifications) /
        (length(y_measures) * length(x_measures) * length(Education_Measures) * length(Economics_Measures) * length(Urbanisation_Measures))
    ),
    times = length(y_measures)
    ),
    times = length(x_measures)
    ),
    times = length(Education_Measures)
    ),
    times = length(Economics_Measures)
    )

  # finally, we return the specification matrix
  Specifications
}


#############################################
# now we perform this function on all four of our variables
#############################################

Specs_IDV <- sca_specifications(x_measures = Courage_Measures, y_measures = Individualism_Measures)
Specs_MAS <- sca_specifications(x_measures = Courage_Measures, y_measures = Masculinity_Measures)
Specs_PDI <- sca_specifications(x_measures = Courage_Measures, y_measures = PowerDistance_Measures)
Specs_UAI <- sca_specifications(x_measures = Courage_Measures, y_measures = UncertaintyAvoidance_Measures)
Specs_LTO <- sca_specifications(x_measures = Courage_Measures, y_measures = LongtermOrient_Measures)
Specs_IDG <- sca_specifications(x_measures = Courage_Measures, y_measures = Indulgence_Measures)
Specs_TLI <- sca_specifications(x_measures = Courage_Measures, y_measures = Tightness_Measures)

Specs_TER <- sca_specifications(x_measures = Courage_Measures, y_measures = Terrorism_Measures)
Specs_INO <- sca_specifications(x_measures = Courage_Measures, y_measures = Innovation_Measures)

Specs_B5O <- sca_specifications(x_measures = Courage_Measures, y_measures = big_five_openness)
Specs_B5C <- sca_specifications(x_measures = Courage_Measures, y_measures = big_five_conscientiousness)
Specs_B5E <- sca_specifications(x_measures = Courage_Measures, y_measures = big_five_extraversion)
Specs_B5A <- sca_specifications(x_measures = Courage_Measures, y_measures = big_five_agreeableness)
Specs_B5N <- sca_specifications(x_measures = Courage_Measures, y_measures = big_five_neuroticism)

Specs_GGI <- sca_specifications(x_measures = Courage_Measures, y_measures = global_giving_index_rank)
Specs_FTO <- sca_specifications(x_measures = Courage_Measures, y_measures = future_orientation)
Specs_HMO <- sca_specifications(x_measures = Courage_Measures, y_measures = humane_orientation)
Specs_PFO <- sca_specifications(x_measures = Courage_Measures, y_measures = performance_orientation)
Specs_RSE <- sca_specifications(x_measures = Courage_Measures, y_measures = rosenberg_self_esteem)

Specs_FTO_P <- sca_specifications(x_measures = Courage_Measures, y_measures = c("GLOBE_future_orientation_societal_practices"))
Specs_HMO_P <- sca_specifications(x_measures = Courage_Measures, y_measures = c("GLOBE_humane_orientation_societal_practices"))
Specs_PFO_P <- sca_specifications(x_measures = Courage_Measures, y_measures = c("GLOBE_performance_orientation_societal_practices"))

################################
# and save the results
#################################

write.csv(Specs_IDV, "results/Specs_IDV.csv")
write.csv(Specs_INO, "results/Specs_INO.csv")
write.csv(Specs_TER, "results/Specs_TER.csv")
write.csv(Specs_MAS, "results/Specs_MAS.csv")

write.csv(Specs_PDI, "results/Specs_PDI.csv")
write.csv(Specs_UAI, "results/Specs_UAI.csv")
write.csv(Specs_LTO, "results/Specs_LTO.csv")
write.csv(Specs_IDG, "results/Specs_IDG.csv")
write.csv(Specs_TLI, "results/Specs_TLI.csv")

write.csv(Specs_B5O, "results/Specs_B5O.csv")
write.csv(Specs_B5C, "results/Specs_B5C.csv")
write.csv(Specs_B5E, "results/Specs_B5E.csv")
write.csv(Specs_B5A, "results/Specs_B5A.csv")
write.csv(Specs_B5N, "results/Specs_B5N.csv")

write.csv(Specs_GGI, "results/Specs_GGI.csv")
write.csv(Specs_FTO, "results/Specs_FTO.csv")
write.csv(Specs_HMO, "results/Specs_HMO.csv")
write.csv(Specs_PFO, "results/Specs_PFO.csv")
write.csv(Specs_RSE, "results/Specs_RSE.csv")

write.csv(Specs_FTO_P, "results/Specs_FTO_P.csv")
write.csv(Specs_HMO_P, "results/Specs_HMO_P.csv")
write.csv(Specs_PFO_P, "results/Specs_PFO_P.csv")
