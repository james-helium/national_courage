#this file is to take in the specification matrix generated and filled by previous files to produce our curves
#plan: import matrices, visualisation function, visualise them, output

library(ggplot2)
library(cowplot)


##################################################
#first, import the filled specification matrix
##################################################

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
  Specifications$y <- gsub('wb','WB',gsub('oecd','OECD',gsub('ipums','IPUMS-I',gsub('un','UN',gsub('_', ' ', Specifications$y)))))
  Specifications$x <- gsub('wb','WB',gsub('oecd','OECD',gsub('ipums','IPUMS-I',gsub('un','UN',gsub('_', ' ', Specifications$x)))))
  Specifications$Control_EDU <- gsub('wb','WB',gsub('oecd','OECD',gsub('ipums','IPUMS-I',gsub('un','UN',gsub('_', ' ', Specifications$Control_EDU)))))
  Specifications$Control_ECN <- gsub('wb','WB',gsub('oecd','OECD',gsub('ipums','IPUMS-I',gsub('un','UN',gsub('_', ' ', Specifications$Control_ECN)))))
  Specifications$Control_URB <- gsub('wb','WB',gsub('oecd','OECD',gsub('ipums','IPUMS-I',gsub('un','UN',gsub('_', ' ', Specifications$Control_URB)))))
  
  #try to plot by groups, first we turn the frame into one that's by group
  #I'm doing this in a for loop because I really can't think of a better way...
  Var.Type <- c()
  Var.Name <- c()
  Specs.Count <- c()
  Specs.Sig <- c()
  for(row in 1:nrow(Specifications)){
    Var.Type <- c(Var.Type, 'Y', 'X', 
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


Plot_IDV <- sca_combine(Specs_IDV, "Individualism")
Plot_MAS <- sca_combine(Specs_MAS, "Masculinity")
Plot_TER <- sca_combine(Specs_TER, "Terrorism")
Plot_INO <- sca_combine(Specs_INO, "Innovation")
Plot_PDI <- sca_combine(Specs_PDI, "Power Distance")
Plot_UAI <- sca_combine(Specs_UAI, "Uncertainty Avoidance")
Plot_LTO <- sca_combine(Specs_LTO, "Longterm Orientation")
Plot_IDG <- sca_combine(Specs_IDG, "Indulgence")
Plot_TLI <- sca_combine(Specs_TLI, "Tightness/Looseness")

# additional variables
Plot_B5O <- sca_combine(Specs_B5O, "Openness")
Plot_B5C <- sca_combine(Specs_B5C, "Conscientiousness")
Plot_B5E <- sca_combine(Specs_B5E, "Extraversion")
Plot_B5A <- sca_combine(Specs_B5A, "Agreeableness")
Plot_B5N <- sca_combine(Specs_B5N, "Neuroticism")
Plot_GGI <- sca_combine(Specs_GGI, "Prosociality")
Plot_FTO <- sca_combine(Specs_FTO, "Future Orientation")
Plot_HMO <- sca_combine(Specs_HMO, "Humane Orientation")
Plot_PFO <- sca_combine(Specs_PFO, "Performance Orientation")
Plot_RSE <- sca_combine(Specs_RSE, "Self-Esteem")

Plot_FTO_P <- sca_combine(Specs_FTO_P, "Future Orientation Practices")
Plot_HMO_P <- sca_combine(Specs_HMO_P, "Humane Orientation Practices")
Plot_PFO_P <- sca_combine(Specs_PFO_P, "Performance Orientation Practices")


###################################################
#EXPORT
###################################################

pdf(file = 'results/SCA_plots.pdf', width = 12, height = 13.5)
Plot_IDV
Plot_MAS
Plot_TER
Plot_INO
Plot_PDI
Plot_UAI
Plot_LTO
Plot_IDG
Plot_TLI
dev.off()

pdf(file = 'results/SCA_plots_additional.pdf', width = 12, height = 13.5)
Plot_B5O
Plot_B5C
Plot_B5E
Plot_B5A
Plot_B5N
Plot_GGI
Plot_FTO
Plot_HMO
Plot_PFO
Plot_RSE
dev.off()

pdf(file = 'results/SCA_added_plots_practices.pdf', width = 12, height = 13.5)
Plot_FTO_P
Plot_HMO_P
Plot_PFO_P
dev.off()
