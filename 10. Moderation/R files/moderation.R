# install and source R functions #######################
  # install.packages("BiocManager")
  # BiocManager::install("rhdf5")
  library(rhdf5)
  source("MplusPlotsUI/mplus.R") 
  # download from https://www.statmodel.com/mplus-R/ "MplusPlotsUI.zip"

# Load the required packages ####################### 
  library(tidyverse); library(magrittr); library(ggplot2)
  library(psych); library(lavaan); 
  library(MplusAutomation); library(misty) # for running Mplus in R
  library(lavaan); library(lavaanPlot); library(lavaanExtra); 
  library(semPlot); library(semTools)

  
# Run models using MplusAutomation functions #######################
  runModels("PCL no interaction 44.inp") # .inp for mplus input file
  runModels("PCL with interaction & J-N 44.inp")
  
# Let's plot in R #######################
  mplus.view.plots("pcl with interaction & j-n 44.gh5") 
  # show the list of functions that we can use
  mplus.plot.loop('pcl with interaction & j-n 44.gh5') 
  # .gh5 is for the graphics

  mplus.plot('pcl with interaction & j-n 44.gh5', plot = "loop", 
             xlab = "NARR", ylab = "PCL")
  # this function is from the "misty" package

# Using modsem package #######################
  # install.packages("modsem")
  library(modsem)  

  # basic interaction model with a plot
  m1 <- '
  # Outer Model
  X =~ x1 + x2 +x3
  Y =~ y1 + y2 + y3
  Z =~ z1 + z2 + z3
  
  # Inner model
  Y ~ X + Z + X:Z 
'
  est1 <- modsem(m1, oneInt)
  summary(est1)
  
  plot_interaction(x = "X", z = "Z", y = "Y", 
                   vals_z = -3:3, model = est1)

  # JN plot example
  m1 <-  ' 
  visual  =~ x1 + x2 + x3 
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9

  visual ~ speed + textual + speed:textual
'
  
  est1 <- modsem(m1, data = lavaan::HolzingerSwineford1939, method = "ca")
  plot_jn(x = "speed", z = "textual", y = "visual", model = est1, max_z = 6)  
  