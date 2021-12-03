########## Assignment one #############
# by the best Zoltan #


library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(dplyr)
library(dbplyr)
library(ggplot2)
library(lm.beta) # for lm.beta	
library(gridExtra) # for grid.arrange	
library(tidyverse) # for tidy format

# RESEARCHQUESTION ONE #

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1") 

AS1 <- data_sample_1

#recoding male and female to male = 0 and female = 1 for better calculations

AS1 = AS1 %>%
  mutate(
    sex2 = recode(sex, 
                  "male" = 0, 
                  "female" = 1)
  )


#looking at data 

############### checking the data for wrong computations ##############

describe(AS1)

summary(AS1)

#we saw, that the max for "pain" is 55, which we assume is a typo that meant to use 5

AS1$pain <- replace(AS1$pain, AS1$pain == 55, 5)

#checking: is okay 


#we saw that the trait anxiety is between 20 to 80, but we have a value of 4.20 

mean_stai = mean(AS1$STAI_trait)

AS1$STAI_trait <- replace(AS1$STAI_trait, AS1$STAI_trait <= 20, mean_stai)

#checking if it worked: it did 
summary(AS1)


############## checking the data for outliers #############
# outliers via plots

AS1 %>%
  mutate(rownum = row.names(AS1)) %>%
  ggplot() + aes(x = age, y = pain, label = rownum) +
  geom_point() + geom_text()


AS1 %>%
  mutate(rownum = row.names(AS1)) %>%
  ggplot() + aes(x = sex, y = pain, label = rownum) +
  geom_point() + geom_text()

#looking for them via boxplots 

ggplot(AS1) +
  aes(x = "values", y = STAI_trait, label = ID) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggplot(AS1) +
  aes(x = "values", y = pain, label = ID) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggplot(AS1) +
  aes(x = "values", y = age, label = ID) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggplot(AS1) +
  aes(x = "values", y = pain_cat, label = ID) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggplot(AS1) +
  aes(x = "values", y = mindfulness, label = ID) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#looking at the outliers for every single predictor variable with the same outcome variable "pain"

AS1 %>%
  mutate(rownum = row.names(AS1)) %>%
  ggplot() + aes(x = STAI_trait, y = pain, label = rownum) +
  geom_point() + geom_text()

AS1 %>%
  mutate(rownum = row.names(AS1)) %>%
  ggplot() + aes(x = pain_cat, y = pain, label = rownum) +
  geom_point() + geom_text()

AS1 %>%
  mutate(rownum = row.names(AS1)) %>%
  ggplot() + aes(x = cortisol_serum, y = pain, label = rownum) +
  geom_point() + geom_text()


AS1 %>%
  mutate(rownum = row.names(AS1)) %>%
  ggplot() + aes(x = mindfulness, y = pain, label = rownum) +
  geom_point() + geom_text()

#outliers via Cooks Distance 

AS1 %>%
  mutate(rownum = row.names(AS1)) %>%
  ggplot() + aes(x = age, y = pain, label = rownum) +
  geom_label()

AS1 %>%
  ggplot() + aes(x = age, y = pain) + geom_point() +
  geom_smooth(method = "lm")

#here we already create the models, although we look at them later 

mod1 = lm(pain ~ age + sex, data = AS1)

mod1

mod2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = AS1)

mod2


#now we look at leverage and Cook's distance 

mod2%>%
  plot(which = 5)

mod2 %>%
  plot(which = 4)

mod1%>%
  plot(which = 5)

mod1 %>%
  plot(which = 4)

#the outliers should have a cook's distance > 4/N, which is 4/160 = 0.025 (which is in my opinion fairly low!)
#the outliers are 8, 23, 47 for mod1, and 47, 65, 86 for mod2, with 47 being in both models and being the "strongest" one 
#we try to exclude them: 

AS1_nooutliers = AS1 %>%
  slice(-c(47))

mod1_nooutliers = lm(pain ~ age + sex, data = AS1_nooutliers)

mod2_nooutliers = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = AS1_nooutliers)

mod1_nooutliers%>%
  plot(which = 5)

mod1_nooutliers %>%
  plot(which = 4)

mod2_nooutliers%>%
  plot(which = 5)

mod2_nooutliers %>%
  plot(which = 4)

# also we checked for the outlier in the data set: its a male participant with a low pain level (value: 2) but we want to keep the entire range, therefore we will keep this participant in the data 




################# checking the ASSUMPTIONS for a regression model #######################
#question: can we assume that if it works for mod2, it also works for mod1, as mod1 is nested in mod2? 

##### normality ##### 

# QQ plot
mod1 %>%
  plot(which = 2)

# QQ plot
mod2 %>%
  plot(which = 2)

#histogram 
residuals_mod2 = enframe(residuals(mod2))
residuals_mod2 %>%
  ggplot() + aes(x = value) + geom_histogram()

# skew and kurtosis	
describe(residuals(mod1))	

# skew and kurtosis	
describe(residuals(mod2))	

shapiro.test(residuals(mod1))

shapiro.test(residuals(mod2))

#skew and kurtosis are in a range around 0, so between -1 and +1 and therefore we can assume normality of residuals! 





############### next assumption is linearity: 

mod1 %>%
  residualPlots()

mod2 %>%
  residualPlots()

#the lines are okay and dont seem very curved 
#the tests return non-significant p-values except for the mod1 , which is almost non.significant, so we assume linearity, as we have a higher order model (mod2) 

###### homoscedasticity ######

mod1 %>%
  plot(which = 3)

    # looks really akward?!

mod2 %>%
  plot(which = 3)

    # looks also weird?? 

mod1 %>%
  ncvTest() # NCV test

mod1 %>%
  bptest() # Breusch-Pagan test

mod2 %>% 
  ncvTest()

mod2 %>%
  bptest()
#non-signficant, therefore not violated! Amazing! 


##### multicollinearity #####

mod1 %>%
  vif()

mod2 %>%
  vif()

# super low vifs, nice! 

########## SUMMARY of assumptions ###############
# normality of residuals: not violated 
# linearity: not violated
# homoscedasticity: not violated
# no multicollinearity: not violated


#build first model 

mod1 = lm(pain ~ age + sex, data = AS1)

mod1

mod2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = AS1)

mod2


#therefore the equation for model one is: 
  # y = 8.15 + (-0.09)*x(age) + 0.3*x(sex) 

  # y = 1.4 + (-0.04)*x(age) + 0.16*x(sex) + (-0.01)*x(STAI_trait) + 0.11*x(pain_cat) + (-0.27)*x(mindfulness) + 0.57*x(cortisolserum)



#comparison 

comp_an = anova(mod1, mod2)

comp_an 

comp_aic = AIC(mod1, mod2)

comp_aic


              
##################### Regression Model ######################


# ### Load custom functions	

# This is a custom function that I wrote which helps in creating the final table for the regression coefficients.	


coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	


#model 1

sm = summary(mod1)	
sm	

AIC(mod1)	

confint(mod1)

lm.beta(mod1)


sm_p_values = as.character(round(sm$coefficients[,4], 3))	
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))	
sm_p_values[sm_p_values == "0"] = "<.001"	


sm_table = cbind(as.data.frame(round(cbind(coef(mod1), confint(mod1), c(0, lm.beta(mod1)$standardized.coefficients[c(2,3)])), 2)), sm_p_values)	
names(sm_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table["(Intercept)","Std.Beta"] = "0"	

# final coefficient table for model 1

sm_table = coef_table(mod1)	
sm_table	




#model 2

sm2 = summary(mod2)	
sm2	

AIC(mod2)	

confint(mod2)

lm.beta(mod2)


sm2_p_values = as.character(round(sm2$coefficients[,4], 3))	
sm2_p_values[sm2_p_values != "0" & sm2_p_values != "1"] = substr(sm2_p_values[sm2_p_values != "0" & sm2_p_values != "1"], 2, nchar(sm2_p_values[sm2_p_values != "0" & sm2_p_values != "1"]))	
sm2_p_values[sm2_p_values == "0"] = "<.001"	


sm2_table = cbind(as.data.frame(round(cbind(coef(mod2), confint(mod2), c(0, lm.beta(mod1)$standardized.coefficients[c(2,3)])), 2)), sm2_p_values)	
names(sm2_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm2_table["(Intercept)","Std.Beta"] = "0"	

# final coefficient table for model 1

sm2_table = coef_table(mod2)	
sm2_table	

