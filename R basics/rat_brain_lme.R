# Chapter 5 Descriptive Syntax in R.

# Read in Rat Brain data in long format.
  rat.brain = read.delim("rat_brainl.txt")

# Attach rat brain data to working memory. 
# attach(rat.brain) 

# Define region and treatment factors.

  rat.brain$region %>% factor %>% summary
  
  rat.brain = 
    rat.brain %>% 
    mutate(region.f = ifelse(region == 3, 0, region) %>% as.factor(),
           treat = (treatment - 1) %>% as.factor())
  
  rat.brain %>% head

###table report on page 221######
###overall ###

  dim(rat.brain)  #obs
 library(psych)
  describe(rat.brain)

  summary(rat.brain$activate) #mean

  sqrt(var(rat.brain$activate)) #stdev

###by treatment group###
  summary(rat.brain$treat)     #obs

  rat.brain %>% 
    group_by(treat) %>% 
    summarize(mean.activate = mean(activate),
              sd.activate = sd(activate))
  

###by region group###
  summary(rat.brain$region.f) #obs
  rat.brain %>% describeBy("region.f")
  
  rat.brain %>% 
    group_by(region.f) %>% 
    summarize(mean.activate = mean(activate),
              sd.activate = sd(activate))
  

###by treat and region group###
 table(rat.brain$treat, rat.brain$region.f)  #obs

 rat.brain %>% 
   group_by(treat, region.f) %>% 
   summarize(mean.activate = mean(activate),
             sd.activate = sd(activate)) 
 
 
 rat.brain = 
   rat.brain %>% 
   group_by(animal) %>% 
   mutate(time = rep(1:max(n())))
 
 rat.brain %>% 
   filter(treatment == 1) %>% 
   ggplot(aes(factor(time), activate, group = animal, color = animal)) + 
   geom_point() + 
   geom_line()
   
 
library(lattice)  # Load the library for trellis graphics.
 trellis.device(color=F) # Make sure color is turned off.

# Load the nlme library, which is required for the 
# plots below as well as for subsequent models.
 library(nlme)

 # rat.brain <- data.frame(rat.brain, rat.brain$region.f, rat.brain$treat)

# Load nlme package.

# Fit Model 5.1.
  model5.1.fit =
    lme(activate ~ region.f*treat,
        random = ~1 | animal, method = "REML", data = rat.brain)

  summary(model5.1.fit)
  anova(model5.1.fit)

# Fit Model 5.2.
  model5.2.fit <- update(model5.1.fit, random = ~ treat | animal)
  
  summary(model5.2.fit)
  anova(model5.2.fit)

  anova(model5.1.fit, model5.2.fit)


# Fit Model 5.3.
  model5.3.fit =
    lme(activate ~ region.f*treat,
                    random = ~ treat | animal,
                    weights = varIdent(form = ~ 1 | treat),
                    data = rat.brain)

  summary(model5.3.fit)

# Likelihood ratio test for Hypothesis 5.2.
  anova(model5.2.fit, model5.3.fit)

# Load lme4 library.
  library(lme4)

# Fit Model 5.1 (see chapter5_R_final.R for code to load the data).
  model5.1.fit.lmer = lmer(activate ~ region.f*treat + (1|animal),
                          data = rat.brain, REML = T)

# View results and 95% confidence intervals.
  summary(model5.1.fit.lmer)
  confint(model5.1.fit.lmer)
  anova(model5.1.fit.lmer)

# Display the random effects (EBLUPs) from the model.
  ranef(model5.1.fit.lmer)

# Fit Model 5.2.
  model5.2.fit.lmer =
    lmer(activate ~ region.f*treat + (treat|animal),
                          data = rat.brain, REML = T)

  summary(model5.2.fit.lmer)
  anova(model5.2.fit.lmer)

# Plot the predicted random effects along with measures of uncertainty.
library(merTools)
  REsim(model5.2.fit.lmer)
  plotREsim(REsim(model5.2.fit.lmer))

# For Hypothesis 5.1.
  0.5*(1 - pchisq(26.1,1)) + 0.5*(1 - pchisq(26.1,2))

