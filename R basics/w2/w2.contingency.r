##########################################
####### week 2 contingency table #########
##########################################

library(haven); library(psych); library(dplyr); 
library(magrittr); library(ggplot2); library(gridExtra)
library(epitools); library(lsr); library(descr)

## 1. input data ##########################
  hivrats = 
    data.frame(
    rat_type = rep(c(1,0), times = c(3,4)),
    tx = c("Active", "Placebo")[c(1,1,2,1,1,2,2)],
    hiv = c(0,1,1,0,1,0,1),
    count = c(30, 20, 15, 10, 10, 15, 35)
    )
  
  # hivrats = 
  #   hivrats[rep(1:7, times = hivrats$count),]
  
  hivrats$rat_type = factor(hivrats$rat_type) 
  hivrats$rat_type %<>% as.factor()
  
  hivrats = 
    hivrats %>% # lets set the var types
    mutate(rat_type = factor(rat_type, levels = c(0, 1)),  
           tx = factor(tx, levels = c("Active", "Placebo")),
           hiv = factor(hiv, levels = c(0, 1)))

## 2. contingency table analysis ##########################
  tmp = xtabs(count ~ tx + hiv, dat = hivrats) # create a contingency table 
  chisq.test(tmp) #   associationTest( ~tx + hiv, dat = hivrats) this works too
  fisher.test(tmp)
  mcnemar.test(tmp)
  riskratio.wald(tmp)
  oddsratio.wald(tmp)
  

## 3. plots ##########################
  p1 = ggplot(data = hivrats, aes(x = tx, fill = hiv)) + 
    geom_bar(position = "stack") + 
    theme_classic()
  p2 = ggplot(data = hivrats, aes(x = tx, fill = hiv)) + 
    geom_bar(position = "fill") + 
    ylab("proportions") + 
    theme_classic()
  
  gridExtra::grid.arrange(p1, p2, ncol = 2)
  
  mosaicplot(tmp, 
             color = c("pink", "lightblue"), main = "Mosaic Plot")
  
  # more information about the plot: 
  # https://steemit.com/programming/@dkmathstats/displaying-table-results-in-r
  