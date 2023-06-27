# this file is to produce initial descriptive analyses

###### IMPORTANT: Manualy add hofstede, innovation, terrorism data prior

###############################
# LIBRARIES AND DATA
###############################

library(apaTables)

setwd("/Users/james_k_he/Desktop/national_courage")
NationalData <- read.csv("data/all_national_data.csv")


###############################
# DESCRIPTIVE STATS TABLES
###############################

# correlation table of courage and IDV MAS measures
IDV_MAS_Courage <- data.frame(
    national_courage = NationalData$national_courage,
    hofstede_idv = NationalData$hofstede_idv,
    ipums_idv = NationalData$ipums_idv,
    hofstede_mas = NationalData$hofstede_mas
)
apa.cor.table(
    data = IDV_MAS_Courage,
    filename = "results/Correlation_IDV_MAS.rtf"
)

# correlation table of major courage measures and TER INO measures
TER_INO_Courage <- data.frame(
    national_courage = NationalData$national_courage,
    global_terrorism_index = NationalData$global_terrorism_index,
    global_innovation_index = NationalData$global_innovation_index
)
apa.cor.table(
    data = TER_INO_Courage,
    filename = "Results/Correlation_TER_INO.rtf"
)

# correlation table of major courage measures and major control variables
CONTROL_Courage_Measures <- data.frame(
    national_courage = NationalData$national_courage,
    un_edu_primary_complete = NationalData$un_edu_primary_complete,
    ipums_edu_adult_literacy = NationalData$ipums_edu_adult_literacy,
    wb_gdp_per_cap_k = NationalData$wb_gdp / 1000,
    wb_urban_population_ratio = NationalData$wb_urban_population_ratio
)
apa.cor.table(
    data = CONTROL_Courage_Measures,
    filename = "Results/Correlation_Controls.rtf"
)


# one major correlation table:
# courage, representative of each control, and representative of each dv
main_corr_measures <- data.frame(
    national_courage = NationalData$national_courage,
    # controls
    education = NationalData$ipums_edu_adult_literacy,
    economics = NationalData$wb_gdp / 1000,
    urbanisation = NationalData$wb_urban_population_ratio,
    # culture dv
    individualism = NationalData$hofstede_idv,
    masculinity = NationalData$hofstede_mas,
    power_distance = NationalData$hofstede_pwd,
    uncertainty_avoidance = NationalData$hofstede_una,
    longterm_orientation = NationalData$hofstede_lto,
    indulgence = NationalData$hofstede_idg,
    tightness_looseness = NationalData$uz_tightloose,
    # outcome dv
    innovation = NationalData$global_innovation_index,
    terrorism = NationalData$global_terrorism_index
)
apa.cor.table(
    data = main_corr_measures,
    filename = "Results/Main_Correlation_Table.rtf"
)
