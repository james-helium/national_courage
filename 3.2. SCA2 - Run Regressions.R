# this file is to take the specification matrix generated in SCA Step 1 and fill it with regression results
# output of this step will then be visualized in step 3 and statistically analyzed in step 4

# plan: import files, write a generic function for this, execute the function on variables, save the results

# we don't need any additional packages for this


##############################
# we will need our main data file and our specification matrices
##############################

National_Data <- read.csv("data/all_national_data.csv")

# and our specification matrices, removing that first weird index column
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
Specs_B5O <- read.csv("results/Specs_B5O.csv")[, -1]
Specs_B5C <- read.csv("results/Specs_B5C.csv")[, -1]
Specs_B5E <- read.csv("results/Specs_B5E.csv")[, -1]
Specs_B5A <- read.csv("results/Specs_B5A.csv")[, -1]
Specs_B5N <- read.csv("results/Specs_B5N.csv")[, -1]

Specs_GGI <- read.csv("results/Specs_GGI.csv")[, -1]
Specs_FTO <- read.csv("results/Specs_FTO.csv")[, -1]
Specs_HMO <- read.csv("results/Specs_HMO.csv")[, -1]
Specs_PFO <- read.csv("results/Specs_PFO.csv")[, -1]
Specs_RSE <- read.csv("results/Specs_RSE.csv")[, -1]

Specs_FTO_P <- read.csv("results/Specs_FTO_P.csv")[, -1]
Specs_HMO_P <- read.csv("results/Specs_HMO_P.csv")[, -1]
Specs_PFO_P <- read.csv("results/Specs_PFO_P.csv")[, -1]


############################
# first, let's create a function to do this generically to save copying times
###############################

# this function will take in the specification matrix
sca_regression <- function(Specifications) {

  # iterate through every row of the specification matrix
  for (row in 1:nrow(Specifications)) {

    # first, let's put the currently iterated specification into a data frame
    regression_data <- data.frame(
      y = National_Data[Specifications$y[row]][, 1],
      x = National_Data[Specifications$x[row]][, 1]
    )
    # for our controls, we will replace the dataset as a string of 0s if it is "Not_Controlling" specification
    if (Specifications$Control_EDU[row] != "Not_Controlling") {
      regression_data <- data.frame(regression_data,
        control_education = National_Data[Specifications$Control_EDU[row]][, 1]
      )
    } else {
      regression_data <- data.frame(regression_data,
        control_education = rep(0, each = 80)
      )
    }

    if (Specifications$Control_ECN[row] != "Not_Controlling") {
      regression_data <- data.frame(regression_data,
        control_economics = National_Data[Specifications$Control_ECN[row]][, 1]
      )
    } else {
      regression_data <- data.frame(regression_data,
        control_economics = rep(0, each = 80)
      )
    }

    if (Specifications$Control_URB[row] != "Not_Controlling") {
      regression_data <- data.frame(regression_data,
        control_urbanised = National_Data[Specifications$Control_URB[row]][, 1]
      )
    } else {
      regression_data <- data.frame(regression_data,
        control_urbanised = rep(0, each = 80)
      )
    }

    # standardizing our data to produce standardized regression coefficient beta, but we don't standardize the 0s
    for (i in 1:ncol(regression_data)) {
      if (all(regression_data[, i] == 0) == FALSE) { # only standardize if all of that column is 0 for controlling reasons
        regression_data[, i] <- scale(regression_data[, i])
      }
    }

    # confirm the colnames for regression
    colnames(regression_data) <- c("y", "x", "control_education", "control_economics", "control_urbanised")


    # drop all rows with NA values
    regression_data <- na.omit(regression_data)

    # run the regression
    if (nrow(regression_data) > 1) {
      regression_result <- lm(
        formula = y ~ x + control_education + control_economics + control_urbanised,
        data = regression_data
      )
      regression_result <- summary(regression_result)


      # now we extract the results of our regression: effect-size as beta, P, SE, R^2, and also N
      # we put them into our specification matrix accordingly
      tryCatch(
        Specifications$Effect_Size[row] <- regression_result$coefficients["x", "Estimate"],
        error = function(e) Specifications$Effect_Size[row] <- NA
      )
      tryCatch(
        Specifications$P_Value[row] <- regression_result$coefficients["x", "Pr(>|t|)"],
        error = function(e) Specifications$P_Value[row] <- NA
      )
      tryCatch(
        Specifications$Standard_Error[row] <- regression_result$coefficients["x", "Std. Error"],
        error = function(e) Specifications$Standard_Error[row] <- NA
      )
      tryCatch(
        Specifications$R_Squared[row] <- regression_result$adj.r.squared,
        error = function(e) Specifications$R_Squared[row] <- NA
      )
      tryCatch(
        Specifications$n[row] <- length(regression_result$residuals),
        error = function(e) Specifications$n[row] <- NA
      )
    }

    # finally, we will print the row number and progress when we finish in each iteration
    print(row)
  }

  # return the specification matrix
  Specifications
}


##############################
# now we use this function on our four variables
#############################

Specs_IDV <- sca_regression(Specs_IDV)
Specs_INO <- sca_regression(Specs_INO)
Specs_MAS <- sca_regression(Specs_MAS)
Specs_TER <- sca_regression(Specs_TER)
Specs_PDI <- sca_regression(Specs_PDI)
Specs_UAI <- sca_regression(Specs_UAI)
Specs_LTO <- sca_regression(Specs_LTO)
Specs_IDG <- sca_regression(Specs_IDG)
Specs_TLI <- sca_regression(Specs_TLI)

# remove rows where n is < 10
Specs_IDV <- Specs_IDV[Specs_IDV$n > 10, ]
Specs_INO <- Specs_INO[Specs_INO$n > 10, ]
Specs_MAS <- Specs_MAS[Specs_MAS$n > 10, ]
Specs_TER <- Specs_TER[Specs_TER$n > 10, ]
Specs_PDI <- Specs_PDI[Specs_PDI$n > 10, ]
Specs_UAI <- Specs_UAI[Specs_UAI$n > 10, ]
Specs_LTO <- Specs_LTO[Specs_LTO$n > 10, ]
Specs_IDG <- Specs_IDG[Specs_IDG$n > 10, ]
Specs_TLI <- Specs_TLI[Specs_TLI$n > 10, ]


# additional variables
Specs_B5O <- sca_regression(Specs_B5O)
Specs_B5C <- sca_regression(Specs_B5C)
Specs_B5E <- sca_regression(Specs_B5E)
Specs_B5A <- sca_regression(Specs_B5A)
Specs_B5N <- sca_regression(Specs_B5N)

Specs_GGI <- sca_regression(Specs_GGI)
Specs_FTO <- sca_regression(Specs_FTO)
Specs_HMO <- sca_regression(Specs_HMO)
Specs_PFO <- sca_regression(Specs_PFO)
Specs_RSE <- sca_regression(Specs_RSE)

Specs_B5O <- Specs_B5O[Specs_B5O$n > 10, ]
Specs_B5C <- Specs_B5C[Specs_B5C$n > 10, ]
Specs_B5E <- Specs_B5E[Specs_B5E$n > 10, ]
Specs_B5A <- Specs_B5A[Specs_B5A$n > 10, ]
Specs_B5N <- Specs_B5N[Specs_B5N$n > 10, ]

Specs_GGI <- Specs_GGI[Specs_GGI$n > 10, ]
Specs_FTO <- Specs_FTO[Specs_FTO$n > 10, ]
Specs_HMO <- Specs_HMO[Specs_HMO$n > 10, ]
Specs_PFO <- Specs_PFO[Specs_PFO$n > 10, ]
Specs_RSE <- Specs_RSE[Specs_RSE$n > 10, ]


Specs_FTO_P <- sca_regression(Specs_FTO_P)
Specs_HMO_P <- sca_regression(Specs_HMO_P)
Specs_PFO_P <- sca_regression(Specs_PFO_P)

Specs_FTO_P <- Specs_FTO_P[Specs_FTO_P$n > 10, ]
Specs_HMO_P <- Specs_HMO_P[Specs_HMO_P$n > 10, ]
Specs_PFO_P <- Specs_PFO_P[Specs_PFO_P$n > 10, ]

############################
# and finally, we save the files
##################################


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
