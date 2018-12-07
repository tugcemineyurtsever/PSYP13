
model3 = lm(pain ~ age + sex +  STAI_trait + cortisol_serum + mindfulness + pain_cat + weight, data = kickout)

#finding multivariate outliers
#Leverage
leverage3 <- hat(model.matrix(model3))
plot(leverage3)
cutleverage3 = (2*7+2) / nrow(kickout)
kickout[leverage3 > cutleverage3,]
badleverage3 = as.numeric(leverage3 > cutleverage3)
order(badleverage3, decreasing = T)[c(7,6,5,4,3,2,1)]
table(badleverage3)
plot(badleverage3)


#Mahalanbobis distance is another way to describe outliers
N <- nrow(kickout)
mahal3 <- (N-1)*(leverage3-1/N)
cutoff3 = qchisq(.999, df=7)
cutoff3
tail(sort(x = mahal3), n =7)

badmahal3 = as.numeric(mahal3 > cutoff3) ##notice the direction of > 
order(badmahal3, decreasing = T)[c(7,6,5,4,3,2,1)]
table(badmahal3)
plot(badmahal3)

#Cooks
#To get Cook's values:
cooks3 = cooks.distance(model3)

#Get the cutoff score:
4 / (N-7-1)
cutcooks3 = 4 / (nrow(kickout) - 7 - 1)
cutcooks3
#Run cutcooks to see the cut off score.
badcooks3 = as.numeric(cooks3 > cutcooks3)
table(badcooks3)
order(badcooks3, decreasing = T)[c(7,6,5,4,3,2,1)]

#I want to create a total score for each participant of outliers.
#So, I add them up for total outlier-ness.
totalout3 = badmahal3 + badleverage3 + badcooks3
table(totalout3)
plot(totalout3)

finalsample <- subset(kickout, totalout3 < 2)

#Remember that top row = their score: 0, 1, 2, 3
#Bottom row is the number of people who have that score. 

output2 <- model3

standardized2 = rstudent(output2)

fitted2 = scale(output2$fitted.values)

#normality
hist(standardized2)

#linearity
qqnorm(standardized2)
abline(0,1)

#homogenity and homoscedasticity
plot(fitted2, standardized2)
abline(0,0)

plot(output2, which = 3)
ncvTest(output2) 
bptest(output2)

par(mfrow = c(2, 2))
plot(model3)
library(broom)
model.diag.metrics <- augment(model3)
head(model.diag.metrics)

# Add observations indices and drop some columns (.se.fit, .sigma) for simplification
model.diag.metrics <- model.diag.metrics %>%
  mutate(index = 1:nrow(model.diag.metrics)) %>%
  select(index, everything(), -.se.fit, -.sigma)

# Inspect the data
head(model.diag.metrics, 4)


# Cook's distance
plot(model3, 4)
# Residuals vs Leverage
plot(model3, 5)

plot(model3, 4, id.n = 5)

model.diag.metrics %>%
  top_n(1, wt = .cooksd)
 
model3. = lm(pain ~ age + sex + STAI_trait + cortisol_serum + mindfulness + pain_cat + weight, data = finalsample)


# backward regression
model3.backward = step(model3., direction = "backward")

summary(model3.backward)

#Reporting stuff
anova(model3.backward, model3.)

summary(model3.)$adj.r.squared
summary(model3.backward)$adj.r.squared

AIC(model3.)
AIC(model3.backward)

confint(model3.)
confint(model3.backward)

lm.beta(model3.)
lm.beta(model3.backward)

pred_modelbir<- predict(model3.)
pred_modeliki <- predict(model3.backward)

RSS_modelbir = sum((finalsample[,"pain"] - pred_modelbir)^2)
RSS_modeliki = sum((finalsample[,"pain"] - pred_modeliki)^2)
RSS_modelbir
RSS_modeliki

############################################################
backwardmodel <- lm(pain ~ age + cortisol_serum + mindfulness + pain_cat, data = finalsample)

theorybasedmodel <- lm(pain ~ age + sex + STAI_trait + cortisol_serum + mindfulness + pain_cat, data = finalsample)

anova(backwardmodel, theorybasedmodel)

summary(backwardmodel)$adj.r.squared
summary(theorybasedmodel)$adj.r.squared

AIC(backwardmodel)
AIC(theorybasedmodel)

confint(backwardmodel)
confint(theorybasedmodel)

lm.beta(backwardmodel)
lm.beta(theorybasedmodel)

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"), 
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))


pred_modelbcm <- predict(backwardmodel)
pred_modeltbm <- predict(theorybasedmodel)

RSS_modelbcm = sum((finalsample[,"pain"] - pred_modelbcm)^2)
RSS_modeltbm = sum((finalsample[,"pain"] - pred_modeltbm)^2)
RSS_modelbcm
RSS_modeltbm

data2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_2.csv")

#fittin

pred_modelnew1 <- predict(backwardmodel)
pred_modelnew2 <- predict(theorybasedmodel)

summary(pred_modelnew1)
summary(pred_modelnew2)

RSS_modelnew1 = sum((data2[,"pain"] - pred_modelnew1)^2)
RSS_modelnew2 = sum((data2[,"pain"] - pred_modelnew2)^2)
RSS_modelnew1
RSS_modelnew2


#####################################
backwardmodel8 <- lm(pain ~ age + cortisol_serum + mindfulness + pain_cat, data = data2)

theorybasedmodel8 <- lm(pain ~ age + sex + STAI_trait + cortisol_serum + mindfulness + pain_cat, data = data2)

summary(backwardmodel8)$adj.r.squared

summary(theorybasedmodel8)$adj.r.squared

summary(backwardmodel8)
summary(theorybasedmodel8)
anova(backwardmodel8, theorybasedmodel8)

summary(backwardmodel8)$adj.r.squared
summary(theorybasedmodel8)$adj.r.squared

AIC(backwardmodel8)
AIC(theorybasedmodel8)

confint(backwardmodel8)
confint(theorybasedmodel8)

lm.beta(backwardmodel8)
lm.beta(theorybasedmodel8)

