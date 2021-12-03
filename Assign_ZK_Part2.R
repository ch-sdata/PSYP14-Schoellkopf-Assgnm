# ASSIGNMENT PART 2

# research question 2 

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


data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1") 

AS2 <- data_sample_1

#recoding male and female to male = 0 and female = 1 for better calculations

AS2 = AS2 %>%
  mutate(
    sex2 = recode(sex, 
                  "male" = 0, 
                  "female" = 1)
  )


#looking at data 

############### checking the data for wrong computations ##############

describe(AS2)

summary(AS2)

#we saw, that the max for "pain" is 55, which we assume is a typo that meant to use 5

AS2$pain <- replace(AS2$pain, AS2$pain == 55, 5)

#checking: is okay 


#we saw that the trait anxiety is between 20 to 80, but we have a value of 4.20 

mean_stai = mean(AS2$STAI_trait)

AS2$STAI_trait <- replace(AS2$STAI_trait, AS2$STAI_trait <= 20, mean_stai)

#checking if it worked: it did 
summary(AS2)



# build model 

fullmodel = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = AS2)

fullmodel


# checking the assumptions: 
  
    # outliers 


fullmodel%>%
  plot(which = 5)

fullmodel %>%
  plot(which = 4)

#dont really see any

    # normality 

# QQ plot
fullmodel %>%
  plot(which = 2)

#histogram 
residuals_mod2 = enframe(residuals(fullmodel))
residuals_mod2 %>%
  ggplot() + aes(x = value) + geom_histogram()


# skew and kurtosis	
describe(residuals(fullmodel))	

shapiro.test(residuals(fullmodel))

# not significant: okay!

    # linearity 

fullmodel %>%
  residualPlots()

#the lines are okay and dont seem very curved 
#the tests return non-significant p-values  

    # homoscedasticity

fullmodel %>%
  plot(which = 3)

fullmodel %>%
  ncvTest() # NCV test

fullmodel %>%
  bptest() # Breusch-Pagan test


#non-signficant, therefore not violated! Amazing! 


    # multicollinearity

fullmodel %>%
  vif()

# vif always < 3 so amazing!!


######### building the model #########

library(stats)

step(object = fullmodel, direction = "backward")

# nice I guess: our final model is 

mod_backward = lm(formula = pain ~ age + pain_cat + mindfulness + cortisol_serum, 
                  data = AS2)

mod_backward

mod_theoryb = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, 
                 data = AS2)

mod_theoryb

# comparison 

comp_2 = anova(mod_backward, mod_theoryb)

comp_2

comp_aic2 = AIC(mod_backward, mod_theoryb)

comp_aic2

# not a sufficient difference between those models! 



# ### Load custom functions	

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


#backwards model 

sm = summary(mod_backward)	
sm	

AIC(mod_backward)	

confint(mod_backward)

lm.beta(mod_backward)


sm_p_values = as.character(round(sm$coefficients[,4], 3))	
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))	
sm_p_values[sm_p_values == "0"] = "<.001"	


sm_table = cbind(as.data.frame(round(cbind(coef(mod_backward), confint(mod_backward), c(0, lm.beta(mod_backward)$standardized.coefficients[c(2,3)])), 2)), sm_p_values)	
names(sm_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table["(Intercept)","Std.Beta"] = "0"	

# final coefficient table for model 1

sm_table = coef_table(mod_backward)	
sm_table	




#theory-based model 

sm2 = summary(mod_theoryb)	
sm2	

AIC(mod_theoryb)	

confint(mod_theoryb)

lm.beta(mod_theoryb)


sm2_p_values = as.character(round(sm2$coefficients[,4], 3))	
sm2_p_values[sm2_p_values != "0" & sm2_p_values != "1"] = substr(sm2_p_values[sm2_p_values != "0" & sm2_p_values != "1"], 2, nchar(sm2_p_values[sm2_p_values != "0" & sm2_p_values != "1"]))	
sm2_p_values[sm2_p_values == "0"] = "<.001"	


sm2_table = cbind(as.data.frame(round(cbind(coef(mod_theoryb), confint(mod_theoryb), c(0, lm.beta(mod_backward)$standardized.coefficients[c(2,3)])), 2)), sm2_p_values)	
names(sm2_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm2_table["(Intercept)","Std.Beta"] = "0"	

# final coefficient table for model 1

sm2_table = coef_table(mod_theoryb)	
sm2_table	







####### testing on new data ##############


data_sample_2 = read.csv("https://tinyurl.com/87v6emky") 

testd <- data_sample_2

# prediction of models on test set 

prediction_backw = predict(mod_backward, testd)

prediction_theoryb = predict(mod_theoryb, testd)

# sum of squares 

RSS_test_backw = sum((testd[,"pain"] - prediction_backw)^2)	

RSS_test_theoryb = sum((testd[,"pain"] - prediction_theoryb)^2)	

RSS_test_backw	
RSS_test_theoryb	



mod_back_mean <- lm(pain ~ 1, data = AS2)

mod_theoryb_mean <- lm(pain ~ 1, data = AS2)

TSS_test_backw = sum((testd[,"pain"] - predict(mod_back_mean))^2)

TSS_test_theoryb = sum((testd[,"pain"] - predict(mod_theoryb_mean))^2)

R2_predicted_backw = 1 - (RSS_test_backw/TSS_test_backw)

R2_predicted_theoryb = 1 - (RSS_test_theoryb/TSS_test_theoryb)

R2_predicted_backw
R2_predicted_theoryb

#the backward regression model has more error than our theory based model 


#the equation for the backward regression is: 
  # y = 1.16 + -0.04*x(age) + 0.11*x(pain_cat) + -0.26*x(mindfulness) + 0.54*x(cortisol_serum)
