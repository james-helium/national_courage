#this file is to take in the specification matrix generated and filled by previous files to produce our curves
#plan: import matrices, visualisation function, visualise them, output

library(ggplot2)
library(cowplot)


##################################################
#first, import the filled specification matrix
##################################################

Specs_INO <- read.csv("results/raw_specification_results/Specs_INO.csv")[, -1]
Specs_TER <- read.csv("results/raw_specification_results/Specs_TER.csv")[, -1]


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


Plot_TER <- sca_combine(Specs_TER, "Terrorism")
Plot_INO <- sca_combine(Specs_INO, "Innovation")


###################################################
#EXPORT
###################################################

pdf(file = 'results/sca_plots/figure_2.pdf', width = 12, height = 13.5)
Plot_INO
dev.off()

pdf(file = 'results/sca_plots/figure_3.pdf', width = 12, height = 13.5)
Plot_TER
dev.off()
