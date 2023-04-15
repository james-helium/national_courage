#this file is to take in the specification matrix generated and filled by previous files to produce our curves
#plan: import matrices, visualisation function, visualise them, output

library(ggplot2)
library(cowplot)


##################################################
#first, import the filled specification matrix
##################################################

Priority <- 1
Specs_IDV <- read.csv(paste("Results/Raw SCA results/Specs_IDV_P", Priority, ".csv", sep = ''))[,-1]
Specs_INO <- read.csv(paste("Results/Raw SCA results/Specs_INO_P", Priority, ".csv", sep = ''))[,-1]
Specs_MAS <- read.csv(paste("Results/Raw SCA results/Specs_MAS_P", Priority, ".csv", sep = ''))[,-1]
Specs_TER <- read.csv(paste("Results/Raw SCA results/Specs_TER_P", Priority, ".csv", sep = ''))[,-1]

#new specs added on 2022-09-28
Specs_PDI <- read.csv(paste("Results/Raw SCA results/Specs_PDI_P", Priority, ".csv", sep = ''))[,-1]
Specs_UAI <- read.csv(paste("Results/Raw SCA results/Specs_UAI_P", Priority, ".csv", sep = ''))[,-1]
Specs_LTO <- read.csv(paste("Results/Raw SCA results/Specs_LTO_P", Priority, ".csv", sep = ''))[,-1]
Specs_IDG <- read.csv(paste("Results/Raw SCA results/Specs_IDG_P", Priority, ".csv", sep = ''))[,-1]
Specs_TGU <- read.csv(paste("Results/Raw SCA results/Specs_TGU_P", Priority, ".csv", sep = ''))[,-1]
Specs_TGE <- read.csv(paste("Results/Raw SCA results/Specs_TGE_P", Priority, ".csv", sep = ''))[,-1]


##################################################
#now we create generic functions to generate visualisations
####################################################

#this function plot the actual curve itself; variable_name should be a string 
sca_curve <- function(Specifications, variable_name){
  #sort by effect size
  Specifications <- Specifications[order(Specifications$Effect_Size),]
  #change "P_Value" to "Sig" as a binary variable to denote significance
  colnames(Specifications)[7] <- 'Sig'
  #change p value to 0 if it's above .05
  Specifications$Sig[Specifications$Sig > 0.05] <- '0'
  #change the rest of them (those that are above 0 now) to 1
  Specifications$Sig[Specifications$Sig > 0] <- '1'
  #add columns for upper and lower bound of the effect sizes
  Specifications <- data.frame(Specifications, 
                               Upper = as.numeric(Specifications$Effect_Size + Specifications$Standard_Error),
                               Lower = as.numeric(Specifications$Effect_Size - Specifications$Standard_Error))
  
  
  ############################
  #Specification Curve
  
  #create a value for the height of our median effect size
  height <- round(median(Specifications$Effect_Size), digits = 3)
  
  #now the plot
  #first, our x-axis represents our specification counts
  plot_curve <- ggplot(data = Specifications, aes(x = 1:nrow(Specifications))) +
    #for the bottom layer, we have the ribbon being the upper/lower bound error range
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "grey70") +
    #above that, we have our actual curve, colored by whether they are significant or not
    geom_point(aes(y = Effect_Size, color = Sig), size = 0.3) + 
    #then we add our median effect size height
    geom_hline(yintercept = height, linetype = "dashed") +
    #and an actual line for the axis, denoting where null effect is
    geom_hline(yintercept = 0) +
    #to color the significance scale we have our own little scale here
    scale_color_manual(values = c("#FF0000", "#000000")) +
    #title for the curve and relevant scale
    ggtitle(paste("Standardised Regression Coefficient of National", variable_name, "Predicted by National Courage")) +
    #we zoom in just for this one (don't generalise the limit!)
    scale_y_continuous(name = "") +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
    )
  

  plot_curve
}

#this function produce the specification panel to go underneath our curve
sca_panel <- function(Specifications, variable_name){
  
  #sort by effect size
  Specifications <- Specifications[order(Specifications$Effect_Size),]
  #change "P_Value" to "Sig" as a binary variable to denote significance
  colnames(Specifications)[7] <- 'Sig'
  #change p value to 0 if it's above .05
  Specifications$Sig[Specifications$Sig > 0.05] <- '0'
  #change the rest of them (those that are above 0 now) to 1
  Specifications$Sig[Specifications$Sig > 0] <- '1'
  #make the variable names more readable
  Specifications$y <- gsub('\\.', ' ', Specifications$y)
  Specifications$x <- gsub('Age Sex', 'Age & Sex', gsub('Weighted', 'Weighted by', gsub('\\.', ' ', gsub('Courage.Mean.', '', Specifications$x))))
  Specifications$Control_EDU <- gsub('\\.', ' ', gsub('X.EDU.', '', gsub('_', ' ', Specifications$Control_EDU)))
  Specifications$Control_ECN <- gsub('\\.', ' ', gsub('X.', '', gsub('_', ' ', Specifications$Control_ECN)))
  Specifications$Control_URB <- gsub('\\.', ' ', gsub('X.URB.', '', gsub('_', ' ', Specifications$Control_URB)))
  
  #try to plot by groups, first we turn the frame into one that's by group
  #I'm doing this in a for loop because I really can't think of a better way...
  Var.Type <- c()
  Var.Name <- c()
  Specs.Count <- c()
  Specs.Sig <- c()
  for(row in 1:nrow(Specifications)){
    Var.Type <- c(Var.Type, 'Y', 'X:\nCourage', 
                  'Control:\nEducation', 'Control:\nEconomics', 'Control:\nUrbanisation')
    Var.Name <- c(Var.Name, Specifications$y[row], Specifications$x[row], 
                  Specifications$Control_EDU[row], Specifications$Control_ECN[row], Specifications$Control_URB[row])
    Specs.Count <- c(Specs.Count, rep(row, 5))
    Specs.Sig <- c(Specs.Sig, rep(Specifications$Sig[row], 5))
    #print for progress
    print(paste(row, 'rows done out of', nrow(Specifications)))
  }
  #combine the individual extractions into one data frame
  spec_panel_data <- data.frame(Var.Type, Var.Name, Specs.Count, Specs.Sig)
  
  
  #plot specifications scattering to correspond with our curve 
  Panel <- ggplot(spec_panel_data, aes(x = Specs.Count, y = Var.Name)) + 
    geom_point(aes(color = Specs.Sig), size = 1) + 
    scale_color_manual(values = c("#FF0000", "#000000")) + 
    facet_grid(Var.Type ~ ., space="free", scales = "free_y") +
    theme(legend.position = "none") + ylab("") + xlab("Specification Rank")
  
  #output
  Panel
}

#another function to combine plots together
sca_combine <- function(Specifications, variable_name){
  curve <- sca_curve(Specifications, variable_name)
  panel <- sca_panel(Specifications, variable_name)
  combined <- plot_grid(curve, panel, ncol = 1, align = 'v', axis = "lr", rel_heights = c(1, 2))
  combined
}


###################################################
#plot them out
###################################################

#since the courage measures that are weighted by age seems to be completely different, we split the visuals into two
Cour_Age_Unweighted <- c('Courage.Mean.Unweighted', 'Courage.Mean.Male.Unweighted', 'Courage.Mean.Female.Unweighted', 
                         'Courage.Mean.Weighted.Sex')
Cour_Age_Weighted <- c('Courage.Mean.Weighted.Age', 'Courage.Mean.Male.Weighted.Age', 'Courage.Mean.Female.Weighted.Age',
                       'Courage.Mean.Weighted.Age.Sex')
#first, the courage measures that are not weighted by age
Plot_IDV_Cour_Age_Unweighted <- sca_combine(Specs_IDV[Specs_IDV$x %in% Cour_Age_Unweighted,], "Individualism")
Plot_MAS_Cour_Age_Unweighted <- sca_combine(Specs_MAS[Specs_MAS$x %in% Cour_Age_Unweighted,], "Masculinity")
Plot_TER_Cour_Age_Unweighted <- sca_combine(Specs_TER[Specs_TER$x %in% Cour_Age_Unweighted,], "Terrorism")
Plot_INO_Cour_Age_Unweighted <- sca_combine(Specs_INO[Specs_INO$x %in% Cour_Age_Unweighted,], "Innovation")
#second, the courage measures that are weighted by age
Plot_IDV_Cour_Age_Weighted <- sca_combine(Specs_IDV[Specs_IDV$x %in% Cour_Age_Weighted,], "Individualism")
Plot_MAS_Cour_Age_Weighted <- sca_combine(Specs_MAS[Specs_MAS$x %in% Cour_Age_Weighted,], "Masculinity")
Plot_TER_Cour_Age_Weighted <- sca_combine(Specs_TER[Specs_TER$x %in% Cour_Age_Weighted,], "Terrorism")
Plot_INO_Cour_Age_Weighted <- sca_combine(Specs_INO[Specs_INO$x %in% Cour_Age_Weighted,], "Innovation")

#new plots from additional dependent variables added on 2022-09-28
Plot_PDI_Cour_Age_Unweighted <- sca_combine(Specs_PDI[Specs_PDI$x %in% Cour_Age_Unweighted,], "Power Distance") 
Plot_UAI_Cour_Age_Unweighted <- sca_combine(Specs_UAI[Specs_UAI$x %in% Cour_Age_Unweighted,], "Uncertainty Avoidance") 
Plot_LTO_Cour_Age_Unweighted <- sca_combine(Specs_LTO[Specs_LTO$x %in% Cour_Age_Unweighted,], "Long Term Orientation") 
Plot_IDG_Cour_Age_Unweighted <- sca_combine(Specs_IDG[Specs_IDG$x %in% Cour_Age_Unweighted,], "Indulgence") 
Plot_TGU_Cour_Age_Unweighted <- sca_combine(Specs_TGU[Specs_TGU$x %in% Cour_Age_Unweighted,], "Tightness (Uz)") 
Plot_TGE_Cour_Age_Unweighted <- sca_combine(Specs_TGE[Specs_TGE$x %in% Cour_Age_Unweighted,], "Tightness (Eriksson)") 

Plot_PDI_Cour_Age_Weighted <- sca_combine(Specs_PDI[Specs_PDI$x %in% Cour_Age_Weighted,], "Power Distance") 
Plot_UAI_Cour_Age_Weighted <- sca_combine(Specs_UAI[Specs_UAI$x %in% Cour_Age_Weighted,], "Uncertainty Avoidance") 
Plot_LTO_Cour_Age_Weighted <- sca_combine(Specs_LTO[Specs_LTO$x %in% Cour_Age_Weighted,], "Long Term Orientation") 
Plot_IDG_Cour_Age_Weighted <- sca_combine(Specs_IDG[Specs_IDG$x %in% Cour_Age_Weighted,], "Indulgence")
Plot_TGU_Cour_Age_Weighted <- sca_combine(Specs_TGU[Specs_TGU$x %in% Cour_Age_Weighted,], "Tightness (Uz)") 
Plot_TGE_Cour_Age_Weighted <- sca_combine(Specs_TGE[Specs_TGE$x %in% Cour_Age_Weighted,], "Tightness (Eriksson)") 



###################################################
#Plot for streamlined 2023-03
###################################################

Plot_IDV_Cour_Streamlined <- sca_combine(Specs_IDV[Specs_IDV$x == "Courage.Mean.Weighted.Age.Sex",], "Individualism")
Plot_MAS_Cour_Streamlined <- sca_combine(Specs_MAS[Specs_MAS$x == "Courage.Mean.Weighted.Age.Sex",], "Masculinity")
Plot_TER_Cour_Streamlined <- sca_combine(Specs_TER[Specs_TER$x == "Courage.Mean.Weighted.Age.Sex",], "Terrorism")
Plot_INO_Cour_Streamlined <- sca_combine(
  Specs_INO[Specs_INO$x == "Courage.Mean.Weighted.Age.Sex" &
              Specs_INO$y == "INO.GII.17_21",], "Innovation")


Plot_IDV_Cour_Streamlined

###################################################
#EXPORT
###################################################

#save in two separate files for greater clarity
pdf(file = 'Results/SCA Unweighted by Age.pdf', width = 16, height = 10)
Plot_IDV_Cour_Age_Unweighted
Plot_MAS_Cour_Age_Unweighted
Plot_TER_Cour_Age_Unweighted
Plot_INO_Cour_Age_Unweighted
dev.off()
pdf(file = 'Results/SCA Weighted by Age.pdf', width = 16, height = 10)
Plot_IDV_Cour_Age_Weighted
Plot_MAS_Cour_Age_Weighted
Plot_TER_Cour_Age_Weighted
Plot_INO_Cour_Age_Weighted
dev.off()

#new plots
pdf(file = 'Results/Additional SCA Unweighted by Age.pdf', width = 16, height = 10)
Plot_PDI_Cour_Age_Unweighted
Plot_UAI_Cour_Age_Unweighted
Plot_LTO_Cour_Age_Unweighted
Plot_IDG_Cour_Age_Unweighted
Plot_TGU_Cour_Age_Unweighted
Plot_TGE_Cour_Age_Unweighted
dev.off()
pdf(file = 'Results/Additional SCA Weighted by Age.pdf', width = 16, height = 10)
Plot_PDI_Cour_Age_Weighted
Plot_UAI_Cour_Age_Weighted
Plot_LTO_Cour_Age_Weighted
Plot_IDG_Cour_Age_Weighted
Plot_TGU_Cour_Age_Weighted
Plot_TGE_Cour_Age_Weighted
dev.off()
