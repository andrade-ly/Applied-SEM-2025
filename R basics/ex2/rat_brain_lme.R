# Chapter 5 Descriptive Syntax in R.

# Read in Rat Brain data in long format.
rat.brain <- read.table("C:/Users/faurot/OneDrive - University of North Carolina at Chapel Hill/15_Teaching/DPET 831/2023/Datasets/rat_brainl.txt", h = T)

# Attach rat brain data to working memory. 
attach(rat.brain) 

# Define region and treatment factors.
region.f <- region
region.f[region == 1] <- 1
region.f[region == 2] <- 2
region.f[region == 3] <- 0
region.f <- factor(region.f)
treat <- treatment
treat[treatment == 1] <- 0
treat[treatment == 2] <- 1
treat <- factor(treat)
rat.brain <- data.frame(rat.brain, region.f, treat)

###table report on page 221######
###overall ###

dim(rat.brain)  #obs
library(psych)
describe(rat.brain)

summary(rat.brain$activate) #mean

sqrt(var(rat.brain$activate)) #stdev

###by treatment group###
summary(treat)     #obs


by(rat.brain$activate,list(treat),summary) #mean

sqrt(by(rat.brain$activate,list(treat),var))   #stdev

###by region group###
summary(region.f) #obs

by(rat.brain$activate,list(region.f),summary) #mean

sqrt(by(rat.brain$activate,list(region.f),var))   #stdev

###by treat and region group###
table(treat,region.f)  #obs

by(rat.brain$activate,list(treat, region.f),summary)  #mean

sqrt(by(rat.brain$activate,list(treat, region.f),var)) #stdev

library(lattice)  # Load the library for trellis graphics.
trellis.device(color=F) # Make sure color is turned off.

# Load the nlme library, which is required for the 
# plots below as well as for subsequent models.
library(nlme)

rat.brain <- data.frame(rat.brain, region.f, treat)

# Load nlme package.
library(nlme)

# Fit Model 5.1.
model5.1.fit <- lme(activate ~ region.f*treat,
                    random = ~1 | animal, method = "REML", data = rat.brain)

summary(model5.1.fit)
anova(model5.1.fit)

# Fit Model 5.2.
model5.2.fit <- update(model5.1.fit, random = ~ treat | animal)

summary(model5.2.fit)
anova(model5.2.fit)

0.5*(1 - pchisq(26.1,1)) + 0.5*(1 - pchisq(26.1,2))

# Fit Model 5.3.
model5.3.fit <- lme(activate ~ region.f*treat,
                    random = ~ treat | animal,
                    weights = varIdent(form = ~ 1 | treat),
                    data = rat.brain)

summary(model5.3.fit)

# Likelihood ratio test for Hypothesis 5.2.
anova(model5.2.fit, model5.3.fit)

# Load lme4 library.
library(lme4)

# Fit Model 5.1 (see chapter5_R_final.R for code to load the data).
model5.1.fit.lmer <- lmer(activate ~ region.f*treat + (1|animal),
                          data = rat.brain, REML = T)

# View results and 95% confidence intervals.
summary(model5.1.fit.lmer)
confint(model5.1.fit.lmer)
anova(model5.1.fit.lmer)

# Display the random effects (EBLUPs) from the model.
ranef(model5.1.fit.lmer)

# Fit Model 5.2.
model5.2.fit.lmer <- lmer(activate ~ region.f*treat + (treat|animal),
                          data = rat.brain, REML = T)

summary(model5.2.fit.lmer)
anova(model5.2.fit.lmer)

# Plot the predicted random effects along with measures of uncertainty.
library(merTools)
REsim(model5.2.fit.lmer)
plotREsim(REsim(model5.2.fit.lmer))

# For Hypothesis 5.1.
0.5*(1 - pchisq(26.1,1)) + 0.5*(1 - pchisq(26.1,2))

