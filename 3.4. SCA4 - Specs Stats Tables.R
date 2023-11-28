# this file is to create some summary statistics tables to aid the interpretation of specification results
# Goals are:
# Create summary tables for each specification curve showing for each variable choice:
# a: number of specifications that has this variable choice
# b: % of specs with this variable that yielded significant result
# c: mean effect size of specs with this variable
# d: mean Rsq value of specs with this variable

################################
# LIBRARIES AND FILES
################################


library(apaTables)

Specs_IDV <- read.csv("results/Specs_IDV.csv")[, -1]
Specs_INO <- read.csv("results/Specs_INO.csv")[, -1]
Specs_MAS <- read.csv("results/Specs_MAS.csv")[, -1]
Specs_TER <- read.csv("results/Specs_TER.csv")[, -1]
Specs_PDI <- read.csv("results/Specs_PDI.csv")[, -1]
Specs_UAI <- read.csv("results/Specs_UAI.csv")[, -1]
Specs_LTO <- read.csv("results/Specs_LTO.csv")[, -1]
Specs_IDG <- read.csv("results/Specs_IDG.csv")[, -1]
Specs_TLI <- read.csv("results/Specs_TLI.csv")[, -1]

# additional variables
Specs_B5O <- na.omit(read.csv("results/Specs_B5O.csv")[, -1])
Specs_B5C <- na.omit(read.csv("results/Specs_B5C.csv")[, -1])
Specs_B5E <- na.omit(read.csv("results/Specs_B5E.csv")[, -1])
Specs_B5A <- na.omit(read.csv("results/Specs_B5A.csv")[, -1])
Specs_B5N <- na.omit(read.csv("results/Specs_B5N.csv")[, -1])

Specs_GGI <- read.csv("results/Specs_GGI.csv")[, -1]
Specs_FTO <- read.csv("results/Specs_FTO.csv")[, -1]
Specs_HMO <- read.csv("results/Specs_HMO.csv")[, -1]
Specs_PFO <- read.csv("results/Specs_PFO.csv")[, -1]
Specs_RSE <- read.csv("results/Specs_RSE.csv")[, -1]

Specs_FTO_P <- na.omit(read.csv("results/Specs_FTO_P.csv")[, -1])
Specs_HMO_P <- na.omit(read.csv("results/Specs_HMO_P.csv")[, -1])
Specs_PFO_P <- na.omit(read.csv("results/Specs_PFO_P.csv")[, -1])

National_Data <- read.csv("data/all_national_data.csv")

#################################
# FUNCTION
#################################

# produce SCA summary tables given an SCA results frame and the desired output file name
sca_table <- function(results, file_name) {

  # first, extract a list of variable names as our row names
  variables <- c()
  constructs <- c()
  for (col in 1:5) {
    variables <- c(variables, unique(results[, col]))
    constructs <- c(constructs, rep(colnames(results)[col], length(unique(results[, col]))))
  }

  # make an empty dataframe to populate results
  output <- data.frame(matrix(data = NA, ncol = 6, nrow = length(variables)))
  colnames(output) <- c("Construct", "Variable Choice", "n(Specification)", "%Significant", "Mean Effect", "Mean R-squard")
  output$Construct <- constructs
  output$`Variable Choice` <- variables

  # populate the output dataframe
  for (row in 1:nrow(output)) {
    this_spec <- subset(results, results[output$Construct[row]] == output$`Variable Choice`[row])

    output$`n(Specification)`[row] <- nrow(this_spec)
    output$`%Significant`[row] <- nrow(subset(this_spec, this_spec$P_Value <= 0.05)) / nrow(this_spec) * 100
    output$`Mean Effect`[row] <- mean(this_spec$Effect_Size)
    output$`Mean R-squard`[row] <- mean(this_spec$R_Squared)
  }

  # improve readability
  output$Construct <- gsub(
    "_EDU", " Education",
    gsub("_ECN", " Economics", gsub("_URB", " Urbanisation", gsub("x", "Courage", constructs)))
  )
  output$`Variable Choice` <- gsub(
    "\\.", " ",
    gsub(
      "_", " ",
      gsub("X.EDU.", "", gsub("X.GDP", "GDP", gsub("X.URB.", "", gsub("Courage.Mean.", "", variables))))
    )
  )

  # output and writefile at the same time
  write.csv(output, paste("results/", file_name, ".csv", sep = ""))
  output
}


###################################
# EXECUTE
###################################

sca_table(Specs_IDV, "SCA_Table_IDV")
sca_table(Specs_MAS, "SCA_Table_MAS")
sca_table(Specs_TER, "SCA_Table_TER")
sca_table(Specs_INO, "SCA_Table_INO")
sca_table(Specs_PDI, "SCA_Table_PDI")
sca_table(Specs_UAI, "SCA_Table_UAI")
sca_table(Specs_LTO, "SCA_Table_LTO")
sca_table(Specs_IDG, "SCA_Table_IDG")
sca_table(Specs_TLI, "SCA_Table_TLI")

sca_table(Specs_B5O, "SCA_Table_B5O")
sca_table(Specs_B5C, "SCA_Table_B5C")
sca_table(Specs_B5E, "SCA_Table_B5E")
sca_table(Specs_B5A, "SCA_Table_B5A")
sca_table(Specs_B5N, "SCA_Table_B5N")

sca_table(Specs_GGI, "SCA_Table_GGI")
sca_table(Specs_FTO, "SCA_Table_FTO")
sca_table(Specs_HMO, "SCA_Table_HMO")
sca_table(Specs_PFO, "SCA_Table_PFO")
sca_table(Specs_RSE, "SCA_Table_RSE")

sca_table(Specs_FTO_P, "SCA_Table_FTO_P")
sca_table(Specs_HMO_P, "SCA_Table_HMO_P")
sca_table(Specs_PFO_P, "SCA_Table_PFO_P")
