rm(list=ls(all=TRUE)) # clears the workspace
graphics.off() # clears graphics

library(Matrix)
library(lme4)
library(stats4)
library(psych) 
library(reshape2) 
library(ggplot2)
library(cAIC4)
library(r2glmm)

data_pain = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_3.csv")


# asign ID as a factor
data_pain$ID = factor(data_pain$ID)
data_pain$age = as.numeric(data_pain$age)
View(data_pain)

#checking the data
describe(data_pain)

hist(data_pain$pain1)
hist(data_pain$pain2)
hist(data_pain$pain3)
hist(data_pain$pain4)

str(data_pain)
### Repeated measures analysis using linear mixed models

## Exploring clustering in the data

# Now lets look at the relatiohsip of the repeated measures of wound healing. (First, we save the variable names of the repeated measures into an object called repeated_variables so that we can easily refer to these variable names later in our functions.) We can explore the correlation between the variables with the cor() function. Notice that the repeated measures data points are highly correlated. This shows that the different observations of wound rating are not independent from each other. This is normal, since the wound rating and the initial size of the incision and the wound healing rate depends on the patient. So this is clustered data. Just like the data in the previous exercise was clustered in classes, here, the data is clustered within participants. 

# designate which are the repeated varibales
repeated_variables = c("pain1", "pain2", "pain3",	"pain4")

# correlation of repeated variables
cor(data_pain[,repeated_variables])

## Reshape dataframe
data_pain_long = melt(data_pain, measure.vars=repeated_variables, variable.name = "time", value.name = "pain_rating_time")

# order data frame by participant ID(not necessary, just makes the dataframe look more intuitive)
data_pain_long$ID = as.numeric(data_pain_long$ID)
data_pain_long = data_pain_long[order(data_pain_long[,"ID"]),]

# change the time variable to a numerical vector
data_pain_long$time = as.numeric(data_pain_long$time)

# Let's explore how this new datafrmae looks like.

View(data_pain_long)
str(data_pain_long)

#Building up the model

mod_rep_int = lmer(pain_rating_time ~ age + sex + STAI_trait + cortisol_serum + mindfulness + pain_cat + time + (1|ID), data = data_pain_long)
mod_rep_slope = lmer(pain_rating_time ~ age + sex + STAI_trait + cortisol_serum + mindfulness + pain_cat + time + (time|ID), data = data_pain_long)

## Comparing models
data_pain_long_withpreds = data_pain_long
data_pain_long_withpreds$pred_int = predict(mod_rep_int)
data_pain_long_withpreds$pred_slope = predict(mod_rep_slope)

# random intercept model
ggplot(data_pain_long_withpreds, aes(y = pain_rating_time, x = time, group = ID))+
geom_point(size = 3)+
geom_line(color='red', aes(y=pred_int, x=time))+
facet_wrap( ~ ID, ncol = 5)

# random slope and intercept model
ggplot(data_pain_long_withpreds, aes(y = pain_rating_time, x = time, group = ID))+
geom_point(size = 3)+
geom_line(color='red', aes(y=pred_slope, x=time))+
facet_wrap( ~ ID, ncol = 5)

# The difference between the predictions of the two models is unremarkable.
# Furthermore, we can compare the cAIC of the two models and use the likelihood ratio test with the anova() function to get further information about the model fit of the two models in comparison to wach other.

cAIC(mod_rep_int)$caic
cAIC(mod_rep_slope)$caic

anova(mod_rep_int, mod_rep_slope)

###########################################
#For the slope model
#BURADAN SONRA EKLEME YAPIYORUM

mod_rep_slope_quad = lmer(pain_rating_time ~ time + I(time^2) + age + sex + STAI_trait + cortisol_serum + mindfulness + pain_cat +(time|ID), data = data_pain_long)

summary(mod_rep_slope_quad)
# And add the predictions to the new dataframe containing the other predicted values as well.
data_pain_long_withpreds$pred_slope_quad = predict(mod_rep_slope_quad)
# Now we can compare the model fit of the random intercept model containing the quadratic effect of time with the random intercept model without it. As usual, we use visualization, cAIC and the likelihood ratio test.

data_pain_long_withpreds$pred_slope_quad = predict(mod_rep_slope_quad)

plot_slope_guad = ggplot(data_pain_long_withpreds, aes(y = pain_rating_time, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope_quad, x=time))+
  facet_wrap( ~ ID, ncol = 5)


plot_slope_guad

cAIC(mod_rep_slope)$caic
cAIC(mod_rep_slope_quad)$caic

anova(mod_rep_slope, mod_rep_slope_quad)

# Let's do this now and refit our model with the centered time and its quadratic term as predictors.

data_pain_long_centered_time_slope = data_pain_long
data_pain_long_centered_time_slope$time_centered = data_pain_long_centered_time_slope$time - mean(data_pain_long_centered_time_slope$time)


mod_rep_slope_quad = lmer(pain_rating_time ~ time_centered + I(time_centered^2) + age + sex + STAI_trait + cortisol_serum + mindfulness + pain_cat + (time|ID), data = data_pain_long_centered_time_slope)

# Now we can request the reportable results the same way we did in the previous exercise. 

# Marginal R squared
r2beta(mod_rep_slope_quad, method = "nsj", data = data_pain_long_centered_time_slope)
??r2beta

# Conditional AIC
cAIC(mod_rep_slope_quad)

# Model coefficients
summary(mod_rep_slope_quad)

# Confidence intervals for the coefficients
confint(mod_rep_slope_quad)
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

# standardized Betas
stdCoef.merMod(mod_rep_slope_quad)

####################################################

## Adding the quadratic term of time to the model

# While exploring the plots we might notice that there is a non-linear relationship between time and wound rating. It seems that wounds improve fast in the first few days, and the healing is slower in the days after that.

# Let's add the quadratic term of time to the model random intercept model tp account for this non-linear relationship.

mod_rep_int_quad = lmer(pain_rating_time ~ time + I(time^2) + age + sex + STAI_trait + cortisol_serum + mindfulness + pain_cat + (1|ID), data = data_pain_long)


# And add the predictions to the new dataframe containing the other predicted values as well.
data_pain_long_withpreds$pred_int_quad = predict(mod_rep_int_quad)
# Now we can compare the model fit of the random intercept model containing the quadratic effect of time with the random intercept model without it. As usual, we use visualization, cAIC and the likelihood ratio test.

data_pain_long_withpreds$pred_int_quad = predict(mod_rep_int_quad)

plot_quad = ggplot(data_pain_long_withpreds, aes(y = pain_rating_time, x = time, group = ID))+
geom_point(size = 3)+
geom_line(color='red', aes(y=pred_int_quad, x=time))+
facet_wrap( ~ ID, ncol = 5)


plot_quad

cAIC(mod_rep_int)$caic
cAIC(mod_rep_int_quad)$caic

anova(mod_rep_int, mod_rep_int_quad)

# Let's do this now and refit our model with the centered time and its quadratic term as predictors.

data_pain_long_centered_time = data_pain_long
data_pain_long_centered_time$time_centered = data_pain_long_centered_time$time - mean(data_pain_long_centered_time$time)


mod_rep_int_quad = lmer(pain_rating_time ~ time_centered + I(time_centered^2) + age + sex + STAI_trait + cortisol_serum + mindfulness + pain_cat + (1|ID), data = data_pain_long_centered_time)

# Now we can request the reportable results the same way we did in the previous exercise. 

# Marginal R squared
r2beta(mod_rep_int_quad, method = "nsj", data = data_pain_long_centered_time)

# Conditional AIC
cAIC(mod_rep_int_quad)

# Model coefficients
summary(mod_rep_int_quad)

# Confidence intervals for the coefficients
confint(mod_rep_int_quad)
stdCoef.merMod <- function(object) {
sdy <- sd(getME(object,"y"))
sdx <- apply(getME(object,"X"), 2, sd)
sc <- fixef(object)*sdx/sdy
se.fixef <- coef(summary(object))[,"Std. Error"]
se <- se.fixef*sdx/sdy
return(data.frame(stdcoef=sc, stdse=se))
}

# standardized Betas
stdCoef.merMod(mod_rep_int_quad)

# Diagnostic model for slope

library(psych) # for pairs.panels
library(ggplot2) # for ggplot
library(reshape2) # for melt function
library(influence.ME) # for influence (this will also load the lme4 package)
library(lattice) # for qqmath

#Preparing the data
# And we center the variable 'time' to avoid problems with multicollinearity.

data_pain_long_centered_time_slope = data_pain_long
data_pain_long_centered_time_slope$time_centered = data_pain_long_centered_time_slope$time - mean(data_pain_long_centered_time_slope$time)

# We also save the sqared time as a new variable. In effect this is one of our fixed effect predictors, and this will save us time later during the model diagnostics. We will call this new variable time_centered_2.

data_pain_long_centered_time_slope$time_centered_2 = data_pain_long_centered_time_slope$time_centered^2

## building the model for slope

# Now we build the final model from the previous exercise. We also create a copy of our data object and save the residuals in a variable we call resid.

mod_rep_slope_quad = lmer(pain_rating_time ~ time_centered + I(time_centered^2) + age + sex + STAI_trait + cortisol_serum + mindfulness + pain_cat + (time|ID), data = data_pain_long_centered_time_slope)

data_pain_long_with_resid_slope = data_pain_long_centered_time_slope
data_pain_long_with_resid_slope$resid = residuals(mod_rep_slope_quad)

influence_observation_slope = influence(mod_rep_slope_quad, obs = T)$alt.fixed # this can take a minute or so
influence_group_slope = influence(mod_rep_slope_quad, group = "ID")$alt.fixed

boxplot(influence_observation_slope[,"time_centered"])
pred_names_slope = colnames(influence_group_slope)

pred_names_slope = colnames(influence_group_slope)

par(mfrow=c(1, length(pred_names)))
for(i in 1:length(pred_names)){
  boxplot(influence_observation_slope[,pred_names_slope[i]], main = pred_names_slope[i])}

for(i in 1:length(pred_names_slope)){
  boxplot(influence_group_slope[,pred_names_slope[i]], main = pred_names_slope[i])}
par(mfrow=c(1,1))

## Normality

# You can check if the residuals on the observation level are normally distributed using the qqmath() or the qqnorm() function.

library(graphics)

qqmath(mod_rep_slope_quad, id = 0.05)


# You can use the same function to draw the QQplot for the random effects. In this case, we only have one random effect, random intercept. The points on the plot should roughly fit on a straight line.

qqmath(ranef(mod_rep_slope_quad))

## Linearity

plot(mod_rep_slope_quad, args = "pearson")

plot(resid ~ time_centered, data = data_pain_long_with_resid_slope)
plot(resid ~ time_centered_2, data = data_pain_long_with_resid_slope)
plot(resid ~ age, data = data_pain_long_with_resid_slope)
plot(resid ~ sex, data = data_pain_long_with_resid_slope)
plot(resid ~ STAI_trait, data = data_pain_long_with_resid_slope)
plot(resid ~ cortisol_serum, data = data_pain_long_with_resid_slope)
plot(resid ~ mindfulness, data = data_pain_long_with_resid_slope)
plot(resid ~ pain_cat, data = data_pain_long_with_resid_slope)

plot(mod_rep_slope_quad, Arg = "pearson")

# When working with mixed linear models we need to check for homoscedasticity across clusters as well.
# We can run a significance test for that by fitting a linear model where we predict the squared residuals with the clustering variable (ID). Check the complete model F-test p-value. If it is < 0.05, heteroscedasticity on the cluster level might be problematic.

homosced_mod_slope = lm(data_pain_long_with_resid_slope$resid^2 ~ data_pain_long_with_resid_slope$ID)
summary(homosced_mod_slpoe)

# You can also inspect the cyclone plot. Here we plot the boxplot of the residuals for each participant, and order these boxes according to the variance of the residal (actually the interquartile range). Here we would expect a gradual increase of the variance of the residual from top to buttom. If the increase is not consistent (some clusters have much larger variance than the previous one on the list), we can suspect heteroscedasticity across clusters/units.

# caluclate interquartile range within each cluster
IQR_of_residuals_by_participant_slope = sapply(split(data_pain_long_with_resid_slope, f = data_pain_long_with_resid_slope$ID), function(x) IQR(x$resid))
# rank ordering them
rank_slope = rank(IQR_of_residuals_by_participant)
# adding rank to the dataframe containing the residuals
data_pain_long_with_resid_slope$rank = rep(rank, each = length(repeated_variables))
# creating a vector of participant IDs ordered based on the rank, this will be used as labels
IDforplot_slope = unique(data_pain_long_with_resid_slope$ID[order(data_pain_long_with_resid_slope$rank)])

# create the cyclone plot

ggplot(data_wound_long_with_resid, aes(y = resid, x = factor(rank), labels = ID))+
geom_boxplot()+
scale_x_discrete(labels=IDforplot)+
coord_flip()

## Multicollinearity

# Finally, we should check for multicollinearity of the fixed effect predictors. Without a well established way to extract the vif from lmer models, we can look at the pariwise correlations of the predictors.
# The correlations don't seem problematic.
# Notice that we see no correlation between time and its quatratic term, because we have centered time.

pairs.panels(data_pain_long_centered_time_slope[,c("time_centered", "time_centered_2", "age", "sex", "STAI_trait", "cortisol_serum", "mindfulness", "pain_cat")], col = "red", lm = T)



# Diagnostic model

library(psych) # for pairs.panels
library(ggplot2) # for ggplot
library(reshape2) # for melt function
library(influence.ME) # for influence (this will also load the lme4 package)
library(lattice) # for qqmath

#Preparing the data
# And we center the variable 'time' to avoid problems with multicollinearity.

data_pain_long_centered_time = data_pain_long
data_pain_long_centered_time$time_centered = data_pain_long_centered_time$time - mean(data_pain_long_centered_time$time)

# We also save the sqared time as a new variable. In effect this is one of our fixed effect predictors, and this will save us time later during the model diagnostics. We will call this new variable time_centered_2.

data_pain_long_centered_time$time_centered_2 = data_pain_long_centered_time$time_centered^2

## building the model

# Now we build the final model from the previous exercise. We also create a copy of our data object and save the residuals in a variable we call resid.

mod_rep_int_quad = lmer(pain_rating_time ~ time_centered + I(time_centered^2) + age + sex + STAI_trait + cortisol_serum + mindfulness + pain_cat + (1|ID), data = data_pain_long_centered_time)

data_pain_long_with_resid = data_pain_long_centered_time
data_pain_long_with_resid$resid = residuals(mod_rep_int_quad)

influence_observation = influence(mod_rep_int_quad, obs = T)$alt.fixed # this can take a minute or so
influence_group = influence(mod_rep_int_quad, group = "ID")$alt.fixed

boxplot(influence_observation[,"time_centered"])
pred_names = colnames(influence_group)

pred_names = colnames(influence_group)

par(mfrow=c(1, length(pred_names)))
for(i in 1:length(pred_names)){
boxplot(influence_observation[,pred_names[i]], main = pred_names[i])}

for(i in 1:length(pred_names)){
boxplot(influence_group[,pred_names[i]], main = pred_names[i])}
par(mfrow=c(1,1))

## Normality

# You can check if the residuals on the observation level are normally distributed using the qqmath() or the qqnorm() function.
??c_palette
library(graphics)
library(c_palette)
qqmath(mod_rep_int_quad, id = 0.05)
qqnorm(mod_rep_int_quad, id = 0.05)

# You can use the same function to draw the QQplot for the random effects. In this case, we only have one random effect, random intercept. The points on the plot should roughly fit on a straight line.

qqmath(ranef(mod_rep_int_quad))

## Linearity

plot(mod_rep_int_quad, arg = "pearson")

plot(resid ~ time_centered, data = data_pain_long_with_resid)
plot(resid ~ time_centered_2, data = data_pain_long_with_resid)
plot(resid ~ age, data = data_pain_long_with_resid)
plot(resid ~ sex, data = data_pain_long_with_resid)
plot(resid ~ STAI_trait, data = data_pain_long_with_resid)
plot(resid ~ cortisol_serum, data = data_pain_long_with_resid)
plot(resid ~ mindfulness, data = data_pain_long_with_resid)
plot(resid ~ pain_cat, data = data_pain_long_with_resid)

plot(mod_rep_int_quad, arg = "pearson")

# When working with mixed linear models we need to check for homoscedasticity across clusters as well.
# We can run a significance test for that by fitting a linear model where we predict the squared residuals with the clustering variable (ID). Check the complete model F-test p-value. If it is < 0.05, heteroscedasticity on the cluster level might be problematic.

homosced_mod = lm(data_pain_long_with_resid$resid^2 ~ data_pain_long_with_resid$ID)
summary(homosced_mod)

# You can also inspect the cyclone plot. Here we plot the boxplot of the residuals for each participant, and order these boxes according to the variance of the residal (actually the interquartile range). Here we would expect a gradual increase of the variance of the residual from top to buttom. If the increase is not consistent (some clusters have much larger variance than the previous one on the list), we can suspect heteroscedasticity across clusters/units.

# caluclate interquartile range within each cluster
IQR_of_residuals_by_participant = sapply(split(data_pain_long_with_resid, f = data_pain_long_with_resid$ID), function(x) IQR(x$resid))
# rank ordering them
rank = rank(IQR_of_residuals_by_participant)
# adding rank to the dataframe containing the residuals
data_pain_long_with_resid$rank = rep(rank, each = length(repeated_variables))
# creating a vector of participant IDs ordered based on the rank, this will be used as labels
IDforplot = unique(data_pain_long_with_resid$ID[order(data_pain_long_with_resid$rank)])

# create the cyclone plot
ggplot(data_pain_long_with_resid, aes(y = resid, x = factor(rank), labels = ID))+
geom_boxplot()+
scale_x_discrete(labels=IDforplot)+
coord_flip()

## Multicollinearity

# Finally, we should check for multicollinearity of the fixed effect predictors. Without a well established way to extract the vif from lmer models, we can look at the pariwise correlations of the predictors.
# The correlations don't seem problematic.
# Notice that we see no correlation between time and its quatratic term, because we have centered time.

pairs.panels(data_pain_long_centered_time[,c("time_centered", "time_centered_2", "age", "sex", "STAI_trait", "cortisol_serum", "mindfulness", "pain_cat")], col = "red", lm = T)

