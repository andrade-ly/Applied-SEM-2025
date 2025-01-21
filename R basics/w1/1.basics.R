##### 1. R basics ######################################################
  tmp = c(1, 2, 3, 4)
  tmp <- c(1, 2, 3, 4)
  tmp == c(1, 2, 3, 4)
  tmp != c(1, 2, 3, 4)

  1+1  
  tmp*4
  
  tmp2 = c("1","2","3","4")
  #tmp2*4 # does this work?
  as.numeric(tmp2)*4  
  
  tmp3 = data.frame(v1 = c(1,2,3,4), v2 = c(5,6,7,8))
  tmp3*4
  tmp3$v1 + tmp3$v2
  
  #?mean # help
  mean(tmp)
  mean(tmp3)
  mean(tmp3[,1])
  mean(tmp3[,2])
  apply(tmp3, 2, mean)
  apply(tmp3, 1, mean)
  colMeans(tmp3)
  rowMeans(tmp3)
  
  class(tmp3)
  str(tmp3)  
  class(tmp3$v1)
  class(tmp3$v2)
  sapply(tmp3, class)
  sapply(tmp3, is.numeric)
  sapply(tmp3, is.na)
  sapply(tmp3, is.factor)  
  
##### 1-2. examples ######################################################
  
  library(psych)
  
  #### descriptive stats ####
  data(sat.act)
  names(sat.act)
  head(sat.act)
  summary(sat.act)
  psych::describe(sat.act)
  describeBy(sat.act, "gender")  
  dim(sat.act) #700 6
  
  #### create summary variables ####
  sat.act$sum.sat = apply(sat.act[,c("SATV", "SATQ")], 1, sum)
  head(sat.act)
  
  #### correlations ####
  tmp.cor = sat.act[1:10,c("ACT", "SATV", "SATQ")] # subsetting data
  corr.test(tmp.cor)
  lowerCor(tmp.cor)
  pairs.panels(tmp.cor)
  

##### 1-3. practice ######################################################
  
  library(haven); library(dplyr); library(magrittr); library(ggplot2)
  dat = read_sas("teeth.sas7bdat")
  dat$Sex %<>% as.factor()
  dat$Ethnicity %<>% as.factor()
  
  dat %>% psych::describe()
  dat %>% psych::describeBy(dat$Sex)
  dat %>% summary
  
  table(dat$Sex)
  table(dat$Nteeth)
  
  table(dat$Sex, dat$Nteeth)
  dat %>% group_by(Sex) %>% 
    summarise(mean.teeth = mean(Nteeth), n = n(), sd = sd(Nteeth))
  
  hist(dat$Nteeth) # some graphics
  dat %>% 
    ggplot(aes(Sex, Nteeth, group = Sex, color = Sex)) + 
    geom_boxplot() + 
    geom_jitter(width = 0.2) + 
    ylab("Number of teeth")  + 
    theme_classic()
  
  dat %>% 
    ggplot(aes(Age, Nteeth, group = Sex, color = Sex)) + 
    geom_point() +
    geom_smooth(method = loess) + 
    theme_classic()
  
  fit = lm(Nteeth ~ Age, data = dat)
  fit %>% summary
  plot(fit)
  
  
    