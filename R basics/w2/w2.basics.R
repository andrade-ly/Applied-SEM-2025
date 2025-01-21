##########################################
####### week 2 frequency table ###########
##########################################


## 0. reading & writing data  #########################
library(haven); library(psych); library(dplyr); library(magrittr); library(ggplot2)
  dat = read_sas("SASlab/teeth.sas7bdat") # read data
  read_dta(); write_dta()
  read_spss(); 

library(readxl)
  read_excel("SASlab/Nteeth.xlsx") # read excel files
  read.csv2(); write.csv2()
  
  write.csv2(dat, "myscv.csv") # practice
  write_sas(dat, "myscv.sas7bdat") # practice
  
  
## 1. basic functions #########################
  # %>% pipe function (chain operator) conveniently links a sequence of anlaysis steps
  dat %>% names # get variable names 
  dat %>% head 
  dat %>% dim # N row & N column
  
  dat$Sex = factor(dat$Sex, levels = c(1, 2), label = c("M","F"))
  dat$Sex %<>% as.factor() # set the variable as a categorical var
  dat$Sex %>% class # factor
  dat$Sex # levels: 1, 2 
  dat$Sex = factor(dat$Sex, levels = c(2, 1)) # you can change the reference with "levels" argument 
  dat = 
    dat %>% mutate(Sex = factor(Sex, levels = c(1, 2))) # this works too! 
  
  dat$Race %<>% as.factor()
  dat$Ethnicity %<>% as.factor()

  sapply(dat, class) # when you have lots of vars this might be easier to check the classes
                     # sapply function let the function run by column
  
  dat[,c("Sex", "Race", "Ethnicity")] = # when you need to change the class of a series of vars
    sapply(dat[,c("Sex", "Race", "Ethnicity")], function(s) factor(s))
  
  dat = # this works too! 
    dat %>% mutate_at(c('Sex', 'Race', 'Ethnicity'), as.factor) 
  
## 2. describe #########################
  dat %>% psych::describe()
  dat %>% psych::describeBy(dat$Sex)
  dat %>% summary
  
  table(dat$Sex) # basic frequency table 
  table(dat$Nteeth)
  
  table(dat$Sex, dat$Nteeth) # basic frequency table by row and column
  dat %>% group_by(Sex) %>% # summarising counts using n = n()
    summarise(mean.teeth = mean(Nteeth), n = n(), sd = sd(Nteeth))
  
  
## 3. some graphics #########################
  hist(dat$Nteeth) # histogram
  dat %>% 
    ggplot(aes(Sex, Nteeth, group = Sex, color = Sex)) + 
    geom_boxplot() + 
    geom_jitter(width = 0.2) + # makes data points not to entirely overlap on top of each other 
    ylab("Number of teeth")  + # y axis label 
    theme_classic() # aesthetic themes
  
  dat %>% 
    ggplot(aes(Age, Nteeth, group = Sex, color = Sex)) + 
    geom_point() +
    geom_smooth(method = loess) + 
    ylab("Number of teeth")  + 
    theme_classic()
  