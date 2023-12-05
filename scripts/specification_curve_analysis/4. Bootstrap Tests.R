#################################
# here we try to do the bootstrap inferencing of SCA as suggested by Simonsohn et al., 2020
########################################

# Simonsohn et al, 2020: the three steps of SCA inference
# 1: test that median(beta) is significantly different from null
# 2: test that n(significant) is significantly higher than null
# 3: test that Stouffer Z is significantly higher than null
# we each specifications are not statistically independent of one another, Simonsohn et al 2020 recommended using Bootstrap hypothesis test

# this analysis script will therefore closely follow the steps suggested by: ibid 2019 supplementary materials:
# https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2694998 - Section 6 "Optional Reading"


################################
# data
#################################

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

######################################
# functions
######################################

# write a function to create a matrix of pseudo dependent variable under the null
get_pseudo_null_df <- function(Specs) {
  # let N be the sample size (80), and K the number of specifications;
  # create an empty data frame of nrow = N, ncol = K + 1 for that 1 is the actual x variable
  null_df <- data.frame(matrix(data = NA, ncol = nrow(Specs) + 1, nrow = nrow(National_Data)))
  colnames(null_df) <- c("x", paste("y*", 1:(ncol(null_df) - 1), sep = ""))
  # extract out our actual X, Y from National Data, and estimates from the Specs frame
  # important: the data need to be standardised first because our betas are standaridised
  actual_x <- scale(National_Data[Specs$x[1]][, 1])
  actual_y <- scale(National_Data[Specs$y[1]][, 1])
  actual_b <- Specs$Effect_Size
  # put the actual x into the first column
  null_df$x <- actual_x
  # now, create psudo y under the null (y* = b times x) and filled into each
  for (i in 2:ncol(null_df)) {
    # have the k number for convinience - this is the #specs' b that we are working with
    k <- i - 1
    # get the y* under the null from this currently iterated specification
    null_df[, i] <- actual_y - actual_x * actual_b[k]
    # print for progress
    print(paste("Generating y* under null:", k, "out of", ncol(null_df) - 1, "specs done"))
  }
  # output the null frame
  null_df
}

# write a function to perform the bootstrap sampling and estimation, outputing a frame summarising each bootstrap for further testing
# input: specification frame, R is the number of repeats we want to bootstrap
get_bootstrap_results <- function(Specs, R = 100) {
  # get null frame from function
  null_frame <- get_pseudo_null_df(Specs)

  # create a result frame and count the number of extreme cases
  bootstrap_results <- data.frame(matrix(data = NA, nrow = R, ncol = 3))
  colnames(bootstrap_results) <- c("Median Effect Size", "Number of p < .05", "Number on Dominant Direction")

  # create an empty bootstrap frames of K col and R rows for each estimations (beta, binary p)
  bootstrap_beta <- data.frame(matrix(data = NA, ncol = ncol(null_frame) - 1, nrow = R))
  bootstrap_sigV <- data.frame(matrix(data = NA, ncol = ncol(null_frame) - 1, nrow = R))
  colnames(bootstrap_beta) <- paste("k", 1:ncol(bootstrap_beta), sep = "")
  colnames(bootstrap_sigV) <- paste("k", 1:ncol(bootstrap_beta), sep = "")

  # populate the results row by row (each row is a random replaced sample of the pseudo data)
  for (row in 1:nrow(bootstrap_beta)) {
    # make a random-with-replacement draw of rows in the null frame
    # store the draw for selecting matching control variables later
    draw <- sample(1:nrow(null_frame), replace = T)
    random_null <- null_frame[draw, ]
    # compute k regression estimate based on this random draw - going through the columns
    for (col in 2:ncol(random_null)) {
      # take out the x-y variable in this specification
      this_spec <- data.frame(x = random_null[, 1], y = random_null[, col])
      # set k (the count of the specs) which is one less than col because col[1] is the x variable
      k <- col - 1

      # we need the control variables from our specs, so this is similar to how we do SCA in previous scripts
      # get education controls
      if (Specs$Control_EDU[k] != "Not_Controlling") {
        this_spec <- data.frame(this_spec, control_education = National_Data[Specs$Control_EDU[k]][draw, 1])
      } else {
        this_spec <- data.frame(this_spec, control_education = rep(0, each = 80))
      }
      # get economic controls
      if (Specs$Control_ECN[k] != "Not_Controlling") {
        this_spec <- data.frame(this_spec, control_economics = National_Data[Specs$Control_ECN[k]][draw, 1])
      } else {
        this_spec <- data.frame(this_spec, control_economics = rep(0, each = 80))
      }
      # get urbanisation controls
      if (Specs$Control_URB[k] != "Not_Controlling") {
        this_spec <- data.frame(this_spec, control_urbanised = National_Data[Specs$Control_URB[k]][draw, 1])
      } else {
        this_spec <- data.frame(this_spec, control_urbanised = rep(0, each = 80))
      }
      # standardizing our data to produce standardized regression coefficient beta, but we don't standardize the 0s
      for (i in 1:ncol(this_spec)) {
        # only standardize if all of that column is 0 for controlling reasons
        if (all(this_spec[, i] == 0) == FALSE) {
          this_spec[, i] <- scale(this_spec[, i])
        }
      }

      # confirm the colnames for regression
      colnames(this_spec) <- c("y", "x", "control_education", "control_economics", "control_urbanised")


      # perform linear regression on it to get the estimator
      regression_result <- lm(
        formula = y ~ x + control_education + control_economics + control_urbanised,
        data = this_spec
      )
      regression_result <- summary(regression_result)

      # extract the beta to put into this slot in the bootstrap frame (row = row, col = k)
      tryCatch(
        bootstrap_beta[row, k] <- regression_result$coefficients["x", "Estimate"],
        error = function(e) {
          bootstrap_beta[row, k] <- NA
        }
      )
      tryCatch(
        bootstrap_sigV[row, k] <- regression_result$coefficients["x", "Pr(>|t|)"],
        error = function(e) {
          bootstrap_sigV[row, k] <- NA
        }
      )
      # and turn the sig into a bianry 1 / 0 at 0.05 cut-off
      if (!is.na(bootstrap_sigV[row, k]) & bootstrap_sigV[row, k] < 0.05) {
        bootstrap_sigV[row, k] <- 1
      } else {
        bootstrap_sigV[row, k] <- 0
      }
      # print for progress
      # var name if for later for-loop usage, can be disregarded
      print(paste(
        "Boostrapping", row, "out of", R,
        "| estimating specification", k, "out of", ncol(random_null) - 1
      ))
    }

    # now we calculate the median beta, number of sig specs, and Stouffer Z to put into bootstrap results frame
    bootstrap_results$`Median Effect Size`[row] <- median(as.numeric(bootstrap_beta[row, ]))
    bootstrap_results$`Number of p < .05`[row] <- sum(as.numeric(bootstrap_sigV[row, ]))
    bootstrap_results$`Number on Dominant Direction`[row] <-
      length(bootstrap_beta[row, ][sign(bootstrap_beta[row, ]) == sign(median(as.numeric(bootstrap_beta[row, ])))])
  }
  # output
  bootstrap_results
}

# write another function to to the actual test
bootstrap_test <- function(Specs_Frame, R = 100) {

  # let's get the median(beta), n(sig), and n(dom) from the Specs frame
  beta_median <- median(Specs_Frame$Effect_Size)
  numb_of_sig <- nrow(subset(Specs_Frame, Specs_Frame$P_Value < 0.05))
  numb_of_dom <- nrow(subset(Specs_Frame, sign(Specs_Frame$Effect_Size) == sign(beta_median)))

  # get the bootstrap result frame from function
  Bootstrap_Frame <- get_bootstrap_results(Specs_Frame, R = R)

  # now we count how many of the bootstrap results are at least as extreme with our observation
  n_larger_effect <- nrow(subset(Bootstrap_Frame, abs(Bootstrap_Frame$`Median Effect Size`) >= abs(beta_median)))
  n_more_sig <- nrow(subset(Bootstrap_Frame, Bootstrap_Frame$`Number of p < .05` >= numb_of_sig))
  n_more_dom <- nrow(subset(Bootstrap_Frame, Bootstrap_Frame$`Number on Dominant Direction` >= numb_of_dom))

  # and we out put this in a dataframe
  output_frame <- data.frame(matrix(data = NA, nrow = 3, ncol = 4))
  colnames(output_frame) <- c("Statistics", "Observation", paste("Number of As-Extreme Results (out of ", R, ")", sep = ""), "P Value")
  output_frame[, 1] <- c("Median Effect Size", "Number of p < .05", "Number on Dominant Direction")
  output_frame[, 2] <- c(beta_median, numb_of_sig, numb_of_dom)
  output_frame[, 3] <- c(n_larger_effect, n_more_sig, n_more_dom)
  output_frame[, 4] <- output_frame[, 3] / R

  # output
  output_frame
}

# write a final wrap function to execute everything and also save files
boot_and_save <- function(Results_Frame, Var_name = "", R = 500) {
  Test_Results <- bootstrap_test(Specs_Frame = Results_Frame, R = R)
  # save the results
  # save bootstrap frames as well in case we need to go back to them
  write.csv(Test_Results, paste("results/Test_Results_", Var_name, ".csv", sep = ""))
  # output display
  Test_Results
}

########################################
# execute significance test and save in one go
# This is the source of Table 4 in the paper
######################################

boot_and_save(Results_Frame = Specs_IDV, Var_name = "IDV")
boot_and_save(Results_Frame = Specs_MAS, Var_name = "MAS")
boot_and_save(Results_Frame = Specs_TER, Var_name = "TER")
boot_and_save(Results_Frame = Specs_INO, Var_name = "INO")
boot_and_save(Results_Frame = Specs_PDI, Var_name = "PDI")
boot_and_save(Results_Frame = Specs_UAI, Var_name = "UAI")
boot_and_save(Results_Frame = Specs_LTO, Var_name = "LTO")
boot_and_save(Results_Frame = Specs_IDG, Var_name = "IDG")
boot_and_save(Results_Frame = Specs_TLI, Var_name = "TLI")

boot_and_save(Results_Frame = Specs_B5O, Var_name = "B5O")
boot_and_save(Results_Frame = Specs_B5C, Var_name = "B5C")
boot_and_save(Results_Frame = Specs_B5E, Var_name = "B5E")
boot_and_save(Results_Frame = Specs_B5A, Var_name = "B5A")
boot_and_save(Results_Frame = Specs_B5N, Var_name = "B5N")

boot_and_save(Results_Frame = Specs_GGI, Var_name = "GGI")
boot_and_save(Results_Frame = Specs_FTO, Var_name = "FTO")
boot_and_save(Results_Frame = Specs_HMO, Var_name = "HMO")
boot_and_save(Results_Frame = Specs_PFO, Var_name = "PFO")
boot_and_save(Results_Frame = Specs_RSE, Var_name = "RSE")

boot_and_save(Results_Frame = Specs_FTO_P, Var_name = "FTO_P")
boot_and_save(Results_Frame = Specs_HMO_P, Var_name = "HMO_P")
boot_and_save(Results_Frame = Specs_PFO_P, Var_name = "PFO_P")