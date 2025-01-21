devtools::install_github("assaforon/table1xls")

# 1 Research Question 1:  What is the effect of exposure to Atrazine through well 
#water on change in hemoglobin from 9 to 36 weeks of pregnancy?


#2 Load and examine the data
#Highest and lowest, outliers, descriptives, scatterplots, distributions
install.packages("readr")
library(readr)
library(dplyr)
library(ggplot2)
install.packages("sjmisc")
library(sjmisc)
install.packages("foreign")
library(labelled)
library(magrittr)
library(tools)
library(tidyr)
library(Hmisc)
library(rms)
getwd()
setwd("C:/Users/faurot/OneDrive - University of North Carolina at Chapel Hill/Analysis_R/teaching")
save(atrazine, file="C:/Users/faurot/OneDrive - University of North Carolina at Chapel Hill/Analysis_R/teaching/atrazine.Rdata")
save(data, file = "data.Rdata")

atrazine <-"C:/Users/faurot/OneDrive - University of North Carolina at Chapel Hill/15_Teaching/DPET 831/2023/Datasets/atrazine.csv"
# reading contents of csv file
atrazine <- read_csv(atrazine, col_names = TRUE)
summary(atrazine)
table(atrazine$edu)
table(atrazine$parity)

#create a hgb change variable
atrazine$hgbchng= atrazine$hgb36 - atrazine$hgb9
summary(atrazine$hgbchng)

install.packages("plotrix")
library(plotrix)
library(haven)
std.error(atrazine$hgb9)
max(atrazine$hgb9)
min(atrazine$hgb9)

athgb<-atrazine$hgb9
hist(athgb, 
     xlab="Hemoglobin at 9 weeks",
     col="lightblue",
     freq=FALSE, 
     xlim=c(8, 14))

summary(atrazine$hgb36)
hist(atrazine$hgb36, 
     xlab="Hemoglobin at 36 weeks",
     col="lightblue",
     freq=FALSE, 
     xlim=c(5, 12))

summary(atrazine$age)
hist(atrazine$age, 
     xlab="Age of woman",
     col="lightblue",
     freq=FALSE, 
     xlim=c(19,31))

summary(atrazine$income)
hist(atrazine$income, 
     xlab="Income distribution",
     col="lightblue",
     freq=FALSE, 
     xlim=c(2,13))

summary(atrazine$wtgain)
hist(atrazine$wtgain, 
     xlab="Weight gain during pregnancy",
     col="lightblue",
     freq=FALSE, 
     xlim=c(24,64))

library(ggplot2)

ggplot(atrazine, aes(x=parity)) +
  geom_bar() + labs(x='parity') + 
  ggtitle("Frequency of number of children") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(atrazine, aes(x=edu)) +
  geom_bar() + labs(x='Education') + 
  ggtitle("Distribution of education") + 
  theme(plot.title = element_text(hjust = 0.8))

ggplot(atrazine, aes(x=hgbchng, group=exposed, fill=exposed)) +
  geom_histogram(position="identity", alpha=0.5) +
  ggtitle("Change in Hemoglobin by exposure") +
  theme_bw()

summary(atrazine$hgbchng)
summary(atrazine$wtgain)
plot(x = atrazine$wtgain, y = atrazine$hgbchng,
     xlab = "Weight gain",
     ylab = "Change in Hemoglobin",
     xlim = c(24, 64),
     ylim = c(1-5, 0),       
     main = "Association between weight gain and change in hemoglobin")

ggplot(atrazine, aes(x = exposed, y = hgb36)) +
  geom_point(aes(color = factor(exposed))) +
  stat_smooth(method = "lm",
              col = "#C42126", se = FALSE, size = 1)


boxplot(hgbchng ~exposed, data=atrazine, 
        xlab="Exposure to atrazine",
        ylab="Hemoglobin change", 
        main="Boxplot of Hemoglobin change")

ggplot(atrazine, aes(x=hgb9, y=hgb36, label = ID, color=exposed)) +  
  geom_point(shape=1) +
  geom_text(aes(label = ID), hjust = - 0.5) 

which(is.na(atrazine))


##ADD A TABLE with the table1 package
#Add labels to your data

atrazine$exposed<-
  factor(atrazine$exposed,
         levels=c(0,1),
         labels=c('Not exposed', 'Atrazine exposed'))
label(atrazine$exposed)<-"Exposed to atrazine"
label (atrazine$age) <-"Maternal age at enrollment"
label(atrazine$hgb9)<-"Hemoglobin at 9 weeks"
label(atrazine$hgb36)<-"Hemoglobin at 36 weeks"
label(atrazine$prenatal)<-"Taking prenatal vitamins"
label(atrazine$psmoke)<-"Smoking history at baseline"
label(atrazine$income)<-"Household income"
label(atrazine$parity)<-"Number of prior births"
label(atrazine$nausea)<-"Nausea of pregancy at 9 weeks"
label(atrazine$edu)<- "Education Level"
label(atrazine$wtgain)<-"Amount of weight gain"
units(atrazine$age)<-"years"
units(atrazine$income)<-"per $10,000"
units(atrazine$wtgain)<-"pounds"

table(atrazine$edu)
atrazine$edu<-
  factor(atrazine$edu,
         levels=c(0,1,2),
         labels=c('<High school', 'High school grad', 'College'))
table(atrazine$psmoke)
atrazine$psmoke<-
  factor(atrazine$psmoke, 
         levels=c(0,1),
         labels=c('Nonsmoker','Smoker'))
table(atrazine$prenatal)
atrazine$prenatal<-
  factor(atrazine$prenatal, 
       levels=c(0,1),
       labels=c('No prenatal vitamins','Taking prenatal vitamins'))

table(atrazine$nausea)
atrazine$nausea<-
factor(atrazine$nausea, 
       levels=c(0,1),
       labels=c('No','Reported Nausea'))
save(atrazine, file = )

library(tableone)
dput(names(atrazine))
myVars<-c( "hgb9", "prenatal", "psmoke", "age", "wtgain", "parity", 
           "edu", "hgb36", "income", "nausea", "exposed")
catVars<-c( "prenatal","psmoke","parity","edu","nausea","exposed")
skewVars<-c("income")
summary(tab1)
print(tab1, nonnormal=skewVars)

tab2<-CreateTableOne(vars=myVars, strata="exposed", data=atrazine, factorVars=catVars)
print(tab2, nonnormal=skewVars)
tab2Mat<-print(tab2, nonnormal=skewVars, quote=FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab2Mat, file = "C:/Users/faurot/OneDrive - University of North Carolina at Chapel Hill/15_Teaching/DPET 831/2023/Datasets/Table1.csv")

## Choose criterion for selecting a model--Research Question 1 
# specifying an ANCOVA approach.
# You want to select the model with the least amount of bias 
#that maximizes precision
#precision is measured by the width of the confidence interval around
#the mean difference

#4 Choose strategy for selecting variables--either use a DAG or 
#remove variables that are not impacting the association 
#between atrazine and change in hemoglobin.
#Otherwise, ignore the covariates.
# at this stage, you can examine variables that you believe might be interacting
#with the primary exposure--enter them and check if any are statistically
#significant.  If not,check for significance as a group with a partial F test
#remove them all if not significant before moving to the next step.

#Conduct the analysis--Let's do it!  Recall that we used a DAG to reduce the
#number of covariates BEFORE moving to modeling.
atrazine$edu=as.factor(atrazine$edu)
res1<-lm(hgb36~exposed+hgb9+parity+edu+psmoke, data=atrazine)
summary(res1)
confint(res1)
# the other approach is to add all potential confounding covariates and try
#to remove those which are not maldistributed across the exposure (see table 1)
res1<-lm(hgb36~exposed+hgb9+parity+edu+psmoke+age+edu+income+nausea, data=atrazine)
summary(res1)
confint(res1)
#take out parity and nausea first--same distribution
res2<-lm(hgb36~exposed+hgb9+edu+psmoke+age+edu+income, data=atrazine)
summary(res2)
confint(res2)
#compare the estimates
pctdiff<-((-0.269484 - -0.26216)/-0.269484)*100 #no appreciable increase in bias
ciwidth_full<-(-0.35270432 - -0.18626332)
ciwidth_red<-(-0.34826268 - -0.17604842) #sadly, no increase in precision--see values to the right
#additionally try to remove income
res3<-lm(hgb36~exposed+hgb9+edu+psmoke+edu+age, data=atrazine)
summary(res3)
confint(res3)
pctdiff2<-((-0.269484 - -0.3570035)/-0.269484)*100 #note that you are looking at the diff from the full
#removing income results in a 32% change--need to use res2 estimates as the least biased.

#Evaluate the model-in this context, model evaluation depends on comparing the estimate for atrazine
#in the full model to the estimate for exposed in the reduced model.  You can check the model diagnostics,
#but they are of less interest compared to predictive modeling.
###############################################################################
##############################################################################

#Research Question 2--What are the predictors of hemoglobin at 9 weeks?
#2) We have examined the data.  Let's also look at the associations between predictors

#examine associations between potential predictors --ADD CORRELATION MATRIX
pairs(~wtgain + income + age, data = atrazine,
      main = "Scatterplot Matrix")

library(psych)
corr.test(atrazine[,c('wtgain', 'income', 'age')]) #correlation 

## ADD EVALUATING NEED TO TRANSFORM OUTCOMES
fit = lm(hgb36 ~ wtgain + income + age, data = atrazine)
plot(fit)

atrazine$log.hgb36 = log(atrazine$hgb36) # log transformation


# 3) Choose criterion--R-squared is often used. 

# 4) Choose strategy for selecting variables ? backward elimination ? other approaches
#ADD IN WHEN AND HOW TO EVALUATE INTERACTION TERMS AND POLYNOMIALS
library(sjmisc)
atrazine$bi.wtgain = dicho(atrazine$wtgain) # creating category var by dichotomizing 
atrazine$bi.income = dicho(atrazine$income) # creating category var by dichotomizing 

?interaction.plot # evaluate interactions 
interaction.plot(
  x.factor = atrazine$bi.wtgain,
  trace.factor = atrazine$bi.income, 
  response = atrazine$hgb36,
  fun = median,
  ylab = " ",
  xlab = " ",
  trace.label = " ",
  col = c("#0198f9", "#f95801"),
  lyt = 1,
  lwd = 3
)

## evaluating polynomial with scatter plot
ggplot(atrazine) + 
  geom_point(aes(wtgain, hgb36)) +
  theme_bw()

## use poly function to add polynomial
fit = lm(hgb36 ~ poly(wtgain, 2) + income + age, data = atrazine)
plot(fit)

# 5) Evaluate model diagnostics.  How can we get the best model to fit the data?
########/For those of you who are interested, the rms package has many features
#that make predictive modeling much easier. 
####See Frank Harrell's book  “Regression Modeling Strategies” 
