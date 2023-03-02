#
pacman::p_load(
  tidyverse,    # data management + ggplot2 graphics
  dplyr,        # select
  ggplot2,      # ggplot2 graphics
  MASS,         # stepwise model , linear discriminant, proportional odds, negative binomial
  skimr,        # get overview of data
  tidymodels,   # for tidy modelling
  survey,       # for survey functions
  srvyr,        # for tidy survey work
  lubridate,    # for converting date character to date format
  tidyquant,    # for tidy time series functions
  patchwork,    # easily combine ggplot rasters
  plyr,         # for seeing the frequency
  freqtables,   # for frequency table
  corrplot,     # for plotting the correlation 
  glue,
  ggpubr,
  car,          # omparison of the regression coefficients
  lmtest,       # package is used to conduct the Wald test
  mice,         # multiple imputation 
  pROC,         # ROC curve
  
  caret,        # for easy machine learning workflow
  gmodels,      # for cross tab percentage
  readxl        #to read xlsx
)
options (scipen=999)

smkluc <- read.csv("C:/Users/humay/Dropbox/HRM/HRM 753/Assignments/Assignment 6/smkluc.csv")

save.image("C:/Users/humay/Dropbox/HRM/HRM 753/Assignments/Assignment 6/smkluc.RData")


#a.calculate the proportion of death by age group and smoking status 

newdata.age.1 <- subset(smkluc, age==1)
sum(newdata.age.1$dead)/sum(newdata.age.1$pop)
#0.03351636 for age group 1 

newdata.age.2 <- subset(smkluc, age==2)
sum(newdata.age.2$dead)/sum(newdata.age.2$pop)
#0.05844382 for age group 2

newdata.age.3 <- subset(smkluc, age==3)
sum(newdata3.age.3$dead)/sum(newdata3.age.3$pop)
#0.09003359 for age group 3

newdata.age.4 <- subset(smkluc, age==4)
sum(newdata.age.4$dead)/sum(newdata.age.4$pop)
#0.1320902 for age group 4

newdata.age.5 <- subset(smkluc, age==5)
sum(newdata.age.5$dead)/sum(newdata.age.5$pop)
#0.1708915 for age group 5

newdata.age.6 <- subset(smkluc, age==6)
sum(newdata.age.6$dead)/sum(newdata.age.6$pop)
#0.2372653 for age group 6

newdata.age.7 <- subset(smkluc, age==7)
sum(newdata.age.7$dead)/sum(newdata.age.7$pop)
#0.3042373 for age group 7

newdata.age.8 <- subset(smkluc, age==8)
sum(newdata.age.8$dead)/sum(newdata.age.8$pop)
#0.3914347  for age group 8

newdata.age.9 <- subset(smkluc, age==9)
sum(newdata.age.9$dead)/sum(newdata.age.9$pop)
#0.4925138

newdata.smoke.1 <- subset(smkluc, smoke==1)
sum(newdata.smoke.1$dead)/sum(newdata.smoke.1$pop)
#0.1588224  for smoke group 1

newdata.smoke.2 <- subset(smkluc, smoke==2)
sum(newdata.smoke.2$dead)/sum(newdata.smoke.2$pop)
#0.2291942 for smoke group 2

newdata.smoke.3 <- subset(smkluc, smoke==3)
sum(newdata.smoke.3$dead)/sum(newdata.smoke.3$pop)
#0.1490737 for smoke group 3

newdata.smoke.4 <- subset(smkluc, smoke==4)
sum(newdata.smoke.4$dead)/sum(newdata.smoke.4$pop)
#0.1677347 for smoke group 4

smkluc$dead
typeof (smkluc$dead)

typeof (smkluc$age)
smkluc$age

smkluc$age.chr= as.character(smkluc$age)
typeof (smkluc$age.chr)

#b.Plot the proportion of dead by age group and by smoking status separately and comment on the observed trend.


#make age as categorical data
smkluc$age.cat= ifelse (smkluc$age == "9","80+",
                        ifelse (smkluc$age == "8","75-79",
                                ifelse (smkluc$age == "7","70-74",
                                        ifelse (smkluc$age == "6","65-69",
                                                ifelse (smkluc$age == "5","60-64",
                                                        ifelse (smkluc$age == "4","55-59",
                                                                ifelse (smkluc$age == "3","50-54",
                                                                        ifelse (smkluc$age == "2","45-49",
                                                                               "40-44"))))))))


comparison.age.age.cat <- cbind (smkluc$age, smkluc$age.cat)
comparison.age.age.cat

#make proportion of the dead 
smkluc$prop.dead = smkluc$dead/smkluc$pop
smkluc$prop.dead
typeof (smkluc$prop.dead)

#plotting the proportion of the dead for age
plot.age.dead= ggplot(smkluc, aes(as.factor(age.cat), prop.dead.perc)) +
geom_bar(stat = "identity", fill="red", width = 0.5)+
  geom_line(aes(x = smkluc$age.cat, y = smkluc$prop.dead.perc), size = 1.5, color="green", group = 1)+
  labs(y = "Proportion of death", x = "Age categories")

plot.age.dead

#make smoke as categorical data
smkluc$smoke

smkluc$smoke.cat=ifelse (smkluc$smoke == "4","cigarettes only",
        ifelse (smkluc$smoke == "3","cigarettes and cigar",
                ifelse (smkluc$smoke == "2","cigars or pipe only",
                        "never smoked")))
smkluc$smoke.cat

#death in percentage 
smkluc$prop.dead.perc=100*smkluc$prop.dead

#plotting the proportion of the dead for smoke
plot.smoke.dead=ggplot(smkluc, aes(x=smoke.cat, y=prop.dead.perc)) +
  geom_bar(stat = "identity", fill="green", width = 0.5)+
   geom_line(aes(x = smkluc$smoke.cat, y = smkluc$prop.dead.perc), size = 1.5, color="red", group = 1)+
  labs(y = "Proportion of death", x = "Smoke categories categories")
 
plot.smoke.dead

#c.	Fit a Logistic, a Poisson, and a Negative binomial regression model 
#considering 'dead' as an outcome, age, and smoking status as exposures.

smkluc$prop.dead
smkluc$dead

#a Logistic 
Log.model <- glm(prop.dead~as.factor(age.cat)+as.factor(smoke.cat), family="binomial", data=smkluc, weight=pop)
Log.model


#Poisson
Poisson.model <- glm(dead~as.factor(age.cat)+as.factor(smoke.cat), family="poisson", data=smkluc)
summary(Poisson.model)


#the conditional means and variances are similar or not
#for age
with(smkluc, tapply(dead, age.cat, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

#for smoke
with(smkluc, tapply(dead, smoke.cat, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

variances(smkluc$age)

#Negative binomial
Neg.bi.model <- glm.nb(dead~as.factor(age.cat)+as.factor(smoke.cat), data=smkluc)
summary(Neg.bi.model)


#d.	Compare and contrast three fitted models. Which model do you choose among the three and why? 

#Note: If the variance is roughly equal to the mean, 
#then a Poisson regression model typically fits a data set well.

#However, if the variance is significantly greater than the mean, 
#then a negative binomial regression model is typically able to fit the data better.


#Residual plot for Poisson regression
p_res <- resid(Poisson.model)
p_res.plot = plot(fitted(Poisson.model), p_res, col='steelblue', pch=16,
     xlab='Predicted death', ylab='Standardized Residuals', main='Poisson')

p_res.plot+abline(0,0)


#Residual plot for negative binomial regression 
nb_res <- resid(Neg.bi.model)
nb_res.plot= plot(fitted(Neg.bi.model), nb_res, col='steelblue', pch=16,
     xlab='Predicted death', ylab='Standardized Residuals', main='Negative Binomial')
abline(0,0)
nb_res.plot+abline(0,0)

#From the plots we can see that the residuals are more spread out for the Poisson regression model (notice that some residuals extend beyond 15) 
#compared to the negative binomial regression model.
#This is a sign that a negative binomial regression model is likely more appropriate since the residuals of that model are smaller. 

#Perform a Likelihood Ratio Test

#Lastly, we can perform a likelihood ratio test to determine if there is a statistically significant difference in the fit of the two regression models:
  
pchisq(2 * (logLik(Neg.bi.model) - logLik(Poisson.model)), df = 1, lower.tail = FALSE)

#The p-value of the test turns out to be <0.001, which is significantly less than 0.05.
#Thus, we would conclude that the negative binomial regression model offers a significantly better fit to the data compared to the Poisson regression model.



##e. Interpret model coefficients from the best-fitted model making any transformation necessary to have a better interpretation.
