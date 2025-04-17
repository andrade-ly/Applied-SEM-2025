# Load the required packages ####################### 
  library(tidyverse); library(magrittr); library(ggplot2)
  library(psych); library(lavaan); 
  library(MplusAutomation); library(misty) # for running Mplus in R
  library(lavaan); library(lavaanPlot); library(lavaanExtra); 
  library(semPlot); library(semTools)
  source("MplusPlotsUI/mplus.R") 

# Run models using MplusAutomation functions #######################
  # Define the model
  # Model with rumination as a mediator
  # rumination (rum) mediates the relationship between 
  #attachment anxiety (att_anx) and fear of rejection (forr)
  
  # Specify the model
  # The model is specified in lavaan syntax
  # The model is a mediation model with rumination as a mediator
  
  # Define the model
  # The model is specified in lavaan syntax

  runModels("Mediation 2.inp") 
  plotobj = semPlot::semPlotModel("Mediation 2.out") # plot the model
  semPaths(plotobj, "est", layout = "tree",
           intercepts = F, residuals = F, fade = F, curvePivot = T,
           nCharNodes = 20, edge.color = "black", edge.width = 0.5)


# Using lavaan  #######################
  # Load the data
  thesis <- read.csv("Thesis.csv", header = TRUE)
  
  # Defining model
  med <- '
        # Measurement Model
        rum =~ rrq5 + rrq6 + rrq1 + rrq3 + rrq2 + rrq4r
        forr =~ tfs10 + tfs2 + tfs9 + tfs1r + tfs5
        att_avo =~ ecr24r + ecr34r + ecr5 + ecr2 + ecr30r
        att_anx =~ ecr3 + ecr36 + ecr6 + ecr1 + ecr11
        att_anx ~~ att_avo
                    
        # Structural Model
        # Direct Effect
        forr ~ c*att_anx # regress outcome IV
                    
        # Mediator
        rum ~ a*att_anx # regress mediator on IV
        forr ~ b*rum # regress outcome on mediator

        # Indirect Effect
        ab := a*b
        
        # Total Effect
        total := c + (a*b)
                    
        # Other Effects
        rum ~ 0*att_avo 
        # specifying no relationship between rumination & attach. avoidance
        '
  
  # Estimate the Model
  med_fit <- 
    sem(model = med,
        data = thesis,
        estimator = "ML",
        meanstructure = TRUE,
        se = "boot",
        bootstrap = 1000) # we are using bootstrap here
  
  parameterEstimates(med_fit, 
                     boot.ci.type = "bca.simple",
                     level = 0.95)

  # Extract the tables
  lavaan_reg(med_fit, nice_table = TRUE)
  lavaan_var(med_fit, nice_table = TRUE)
  lavaan_cov(med_fit, nice_table = TRUE)
  lavaan_cov(med_fit, nice_table = TRUE)
  lavaan_defined(med_fit,
                 lhs_name = "Indirect Effect", nice_table = TRUE)
  
  # You can also use the following codes with "operator"
  lavaan_extract(med_fit, operator = "=~", nice_table = TRUE)
  lavaan_extract(med_fit, operator = "~", nice_table = TRUE)
  lavaan_extract(med_fit, operator = "~~", nice_table = TRUE)
  lavaan_extract(med_fit, operator = "~1", nice_table = TRUE)
  
  # Plot 
  semPaths(med_fit, "est", layout = "tree",
           intercepts = F, residuals = F, fade = F, curvePivot = T,
           nCharNodes = 20, edge.color = "black", edge.width = 0.5)
  
  