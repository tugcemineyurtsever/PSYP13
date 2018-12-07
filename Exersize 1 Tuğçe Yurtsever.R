
library(psych)
library(dplyr)
library(gsheet) 
library(ggplot2)
library(car)
library(rgl)
library(lm.beta)
library(errors)
library(lmtest) 
library(sandwich)


View(home_sample_1)
summary(home_sample_1)

# Display simple descriptive statistics and plots.
# We are going to predict pain using the variables age and sex.


describe(home_sample_1)
table(home_sample_1$sex)

cleaned_home_sample_1 <- home_sample_1
# you should exclude or correct data that is not valid


cleaned_home_sample_1["age"] = as.numeric(c(cleaned_home_sample_1["age"]))
cleaned_home_sample_1[cleaned_home_sample_1["age"] == 222, "age"] = 22

describe(cleaned_home_sample_1["age"])

describe(cleaned_home_sample_1$age)


# histograms
hist(cleaned_home_sample_1$pain)

hist(cleaned_home_sample_1$age)

# scatterplots

plot(pain ~ age, data = cleaned_home_sample_1)

table(cleaned_home_sample_1$sex)
plot(cleaned_home_sample_1$pain ~ cleaned_home_sample_1$age)

sample2 <- cleaned_home_sample_1

newsample2 <- sample2[-c(112, 146), ]

summary(newsample2)

levels(newsample2$sex)

newsample2$sex=as.factor(newsample2$sex)

str(newsample2)

##########################################################
#                                                         #
#                   Multiple regression                   #
#                                                         #
###########################################################

# We fit a regression model with multiple predictors: age and sex. In the formula, the predictors are separated by a + sign.

mod_mean <- lm(pain ~ 1, data = newsample2)
mod_mean

mod1 = lm(pain ~ age + sex, data = newsample2)

# tilde

# The regression equation is displayed just like in the case of simple regression

#regression equation Y= b0+ b1*age+ b2*sex
mod1

summary(mod1)

# It is not trivial to visualize the regression equation in multiple regression. You can plot every simple regression separately, but that is not an accurate depiction of the prediction using the model.

plot(pain ~ age, data = newsample2)
abline(lm(pain ~ age, data = newsample2))
plot(newsample2$pain ~ newsample2$sex, data = newsample2)
abline(lm(pain ~ sex, data = newsample2))

library(nlme)
library(mgcv)

# Alternatively, you can use a 3 dimensional plot to visualize the regression plane.


# the regression equation: Y = b0 + b1*X1 + b2*X2


plot(pain ~ age, data = newsample2)
abline(mod1)
mod2 = lm(pain ~ age + sex + STAI_trait + cortisol_serum + cortisol_saliva + mindfulness + pain_cat, data = newsample2)

summary(mod2)

pred_mod1 <- predict(mod1)
pred_mod2 <- predict(mod2)

#it gave an error "Error in sample2[, "pain"] - mod2 : non-numeric argument to binary operator"

RSS_mod1 = sum((newsample2[,"pain"] - pred_mod1)^2)
RSS_mod2 = sum((newsample2[,"pain"] - pred_mod2)^2)
RSS_mod1
RSS_mod2

summary(mod1)$adj.r.squared

summary(mod2)$adj.r.squared

# Now, we should compare residual error and model fit thought the anova() function and the AIC() function.
# The anova() function can only be used for comparing models if the two models are "nested", that is, predictors in one of the models are a subset of predictors of the other model.
# If the anova F test is significant, it means that the models are significantly different in terms of their residual errors.
# If the difference in AIC of the two models is larger than 2, the two models are significantly different in their model fit. Smaller AIC means less error and better model fit, so in this case we accept the model with the smaller AIC. However, if the difference in AIC does not reach 2, we can retain either of the two models. Ususally we stick with the less complicated model in this case, but theoretical considerations and previous results should also be considered when doing model selection.

anova(mod1, mod2)

AIC(mod1)
AIC(mod2)

confint(mod1)
confint(mod2)

lm.beta(mod1)
lm.beta(mod2)


#finding multivariate outliers
#Leverage
leverage <- hat(model.matrix(mod2))
plot(leverage)
cutleverage = (2*7+2) / nrow(newsample2)
newsample2[leverage> cutleverage,]
badleverage = as.numeric(leverage > cutleverage)
table(badleverage)

#Mahalanbobis distance is another way to describe outliers
N <- nrow(newsample2)
mahal <- (N-1)*(leverage-1/N)
cutoff = qchisq(.999, df=7)
order(mahal, decreasing = T)[c(7,6,5,4,3,2,1)]
badmahal = as.numeric(mahal > cutoff) ##notice the direction of > 
table(badmahal)

#Cooks
#To get Cook's values:
cooks = cooks.distance(mod2)

#Get the cutoff score:
4 / (N-7-1)
cutcooks = 4 / (nrow(newsample2) - 7 - 1)
#Run cutcooks to see the cut off score.
badcooks = as.numeric(cooks > cutcooks)
table(badcooks)

#We want to create a total score for each participant of outliers.
#So, we add them up for total outlier-ness.
totalout = badmahal + badleverage + badcooks
table(totalout)

#Remember that top row = their score: 0, 1, 2, 3
#Bottom row is the number of people who have that score. 
plot(totalout)
output <- mod2
noout = subset(newsample2, totalout < 2)


standardized = rstudent(output)

fitted = scale(output$fitted.values)

#normality
hist(standardized)

#linearity
qqnorm(standardized)
abline(0,1)

#homogenity and homoscedasticity
plot(fitted, standardized)
abline(0,0)

plot(output, which = 3)
ncvTest(output) 
bptest(output)

par(mfrow = c(2, 2))
plot(mod2)
library(broom)
model.diag.metrics <- augment(mod2)
head(model.diag.metrics)
# Add observations indices and
# drop some columns (.se.fit, .sigma) for simplification
model.diag.metrics <- model.diag.metrics %>%
  mutate(index = 1:nrow(model.diag.metrics)) %>%
  select(index, everything(), -.se.fit, -.sigma)
# Inspect the data
head(model.diag.metrics, 4)


# Cook's distance
plot(mod2, 4)
# Residuals vs Leverage
plot(mod2, 5)

plot(mod2, 4, id.n = 5)

model.diag.metrics %>%
  top_n(4, wt = .cooksd)
# excludeing variables 

model1 = lm(pain ~ age + sex, data = noout)
model2 = lm(pain ~ age + sex + STAI_trait + cortisol_serum + cortisol_saliva + mindfulness + pain_cat, data = noout)

summary(model2)
summary(mod2)
#not gonna excludsng nything.
library(VIF)
#additivity 

#according to correlation results cortisol salvia and cortisol other is correlated on .89 so I will exclude salvia

model2. = lm(pain ~ age + sex + STAI_trait + cortisol_serum + mindfulness + pain_cat, data = noout)

summary(model2.)
summary(model1)

anova(model1, model2.)

AIC(model1)
AIC(model2.)

confint(model1)
confint(model2.)

lm.beta(model1)
lm.beta(model2.)

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"), 
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))


pred_mod1 <- predict(model1)
pred_mod2 <- predict(model2.)

RSS_mod1 = sum((noout[,"pain"] - pred_mod1)^2)
RSS_mod2 = sum((noout[,"pain"] - pred_mod2)^2)
RSS_mod1
RSS_mod2

summary(model1)$adj.r.squared

summary(model2.)$adj.r.squared

#FINAL CHECK
#finding multivariate outliers
#Leverage
leverage2 <- hat(model.matrix(model2.))
plot(leverage2)
cutleverage2 = (2*6+2) / nrow(noout)
noout[leverage2> cutleverage2,]
badleverage2 = as.numeric(leverage2 > cutleverage2)
table(badleverage2)

#Mahalanbobis distance is another way to describe outliers
N <- nrow(noout)
mahal2 <- (N-1)*(leverage2-1/N)
cutoff2 = qchisq(.999, df=6)
order(mahal2, decreasing = T)[c(6,5,4,3,2,1)]
badmahal2 = as.numeric(mahal2 > cutoff2) ##notice the direction of > 
table(badmahal2)

#Cooks
#To get Cook's values:
cooks2 = cooks.distance(model2.)

#Get the cutoff score:
4 / (N-6-1)
cutcooks2 = 4 / (nrow(noout) - 6 - 1)
#Run cutcooks to see the cut off score.
badcooks2 = as.numeric(cooks2 > cutcooks2)
table(badcooks2)

#We want to create a total score for each participant of outliers.
#So, we add them up for total outlier-ness.
totalout2 = badmahal2 + badleverage2 + badcooks2
table(totalout2)

#Remember that top row = their score: 0, 1, 2, 3
#Bottom row is the number of people who have that score. 
plot(totalout2)

kickout = subset(noout, totalout2 < 2)


standardized2 = rstudent(model2.)

fitted2 = scale(model2.$fitted.values)

#normality
hist(standardized2)

#linearity
qqnorm(standardized2)
abline(0,1)

#homogenity and homoscedasticity
plot(fitted2, standardized2)
abline(0,0)

plot(model2., which = 3)
ncvTest(model2.) 
bptest(model2.)

model1son = lm(pain ~ age + sex, data = kickout)
model2son = lm(pain ~ age + sex + STAI_trait + cortisol_serum + mindfulness + pain_cat, data = kickout)

summary(model1son)
summary(model2son)

anova(model1son, model2son)

AIC(model1son)
AIC(model2son)

confint(model1son)
confint(model2son)

lm.beta(model1son)
lm.beta(model2son)

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"), 
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))


pred_model1son <- predict(model1son)
pred_model2son <- predict(model2son)

RSS_model1son = sum((kickout[,"pain"] - pred_mod1)^2)
RSS_model2son = sum((kickout[,"pain"] - pred_mod2)^2)
RSS_model1son
RSS_model2son

summary(model1son)$adj.r.squared

summary(model2son)$adj.r.squared