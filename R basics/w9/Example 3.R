three =
  data.frame(
    Id = c(1:19),
    age = c(24, 36, 28, 25, 26, 22, 27, 27, 36, 24, 26, 29, 33, 31, 30, 22, 27, 46, 36),
    sex = c("M", "M", "F", "M", "F", "M", "M", "M", "M", "M", "M", "M", "F", "M", "M", "M", "M", "M", "M"),
    height = c(175, 172, 171, 166, 166, 176, 185, 171, 185, 182, 180, 163, 180, 180, 180, 168, 168, 178, 173),
    weight = c(78, 67.6, 98, 65.5, 65, 65.5, 85.5, 76.3, 79, 88.2, 70.5, 75, 68, 65, 70.4, 63, 91.2, 67, 62),
    fev1 = c(4.7, 4.3, 3.5, 4, 3.2, 4.7, 4.3, 4.7, 5.2, 4.2, 3.5, 3.2, 2.6, 2, 4, 3.9, 3, 4.5, 2.4)
  )

summary(three)

library(sjmisc)
three$bi.height = dicho(three$height)

# check for interaction
interaction.plot(
  x.factor = three$bi.height,
  trace.factor = three$sex, 
  response = three$fev1,
  fun = median,
  ylab = " ",
  xlab = " ",
  trace.label = " ",
  col = c("#0198f9", "#f95801"),
  lyt = 1,
  lwd = 3
)

fit1 = lm(fev1 ~ sex + height, data = three) # model without interaction
fit2 = lm(fev1 ~ sex*height, data = three) # model with interaction
anova(fit1, fit2) # partial F test (no difference! We might not need the interaction term)

# check for polynomial nonlinear relationsihps
three %>% 
  ggplot(aes(age, fev1)) + 
  geom_point() +
  geom_smooth()

fit3 = lm(fev1 ~ age, data = three)
fit4 = lm(fev1 ~ poly(age, 2), data = three)
anova(fit3, fit4) # partial F test




