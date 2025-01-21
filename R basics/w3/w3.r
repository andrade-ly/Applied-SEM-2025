##########################################
############# week 3 exercise ############
##########################################
library(haven); library(psych); library(dplyr); 
library(magrittr); library(ggplot2); library(gridExtra)
library(epitools); library(lsr); library(descr); library(epiR); library(epiDisplay)



## 0. data step ###############################
  dat = read_sas("SASlab/Choices.sas7bdat") # load data 
  names(dat) # name of the variables
  dat %>% dim # 2536, 16
  summary(dat) # get min, max, NA's
  
## 1. Create a new variable (pref2cat) ###############################
  dat = 
    dat %>% 
    mutate(pref2cat = ifelse(pref >=3, 1, 0),
           gender = ifelse(gender == 1, "F", "M"),
           race = ifelse(race == 1, "W", "B"))

## 2. Define the percentage of Black men and Black women #############
  tab = table(dat$race, dat$gender) 
  tab[1,2]/sum(tab)*100 # percentage of black man
  tab[1,1]/sum(tab)*100 # percentage of blakc women 
  
## 3. Calculate the difference in proportion between men and women for prefcat=1 vs prefcat=0 
  ## with a 95% confidence interval (Risk/prevalence difference)
  cont = xtabs( ~ gender + pref2cat, dat = dat) # create a contingency table 
  riskratio(cont) # difference in proportion: 434/1556*100 - 292/980*100
  riskratio(dat$gender, dat$pref2cat) # this works too
  
  prop.test(x = c(434, 292), n = c(1556, 980))
  
## 4. Conduct a chi square test for with gender and prefcat ##############
  chisq.test(cont)

## 5. Maental-Haenzel stratified analysis  
  mhor(dat$gender, dat$pref2cat, dat$race)
  
## 6. Conduct an independent samples t-test comparing the mean preference score between men and women 
  ## with reporting of the mean difference and its confidence interval. 
  
  t.test(pref ~ gender, data = dat) # outcome ~ predictor
  
## 7. Conduct a paired t-test comparing pref and fpref in the dataset 
  # t.test(before, after, paired = TRUE)
  t.test(dat$fpref, dat$pref, paired = TRUE)
  
  
  

  