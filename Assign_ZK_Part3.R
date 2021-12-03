#Assignment Part 3

AS3 <- read.csv("https://tinyurl.com/b385chpu")

AS4 <- read.csv("https://tinyurl.com/4f8thztv")


library(psych) # for pairs.panels
library(influence.ME) # for influence (this will also load the lme4 package)
library(lattice) # for qqmath
library(lme4) # for mixed models
library(lmerTest) # for significance test on lmer() mixed models
library(tidyverse) # for tidy code and ggplot\t
library(cAIC4) # for cAIC\t
library(r2glmm) # for r2beta\t
library(lmerTest) # to get singificance test in lmer
library(MuMIn) # for r.squaredGLMM
library(dplyr)
library(psych) # for describe		
library(cAIC4) # for cAIC		
library(r2glmm) # for r2beta		
library(lme4) # for lmer	
library(lmerTest) # to get singificance test in lmer	
library(MuMIn) # for r.squaredGLMM	
library(tidyverse) # for tidy code and ggplot		


# function 



stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	


# DATA

AS3 = AS3 %>% 
  mutate(ID = factor(ID), sex = factor(sex), hospital = factor(hospital))

AS3 = AS3 %>%
  mutate(
    hospital_nr = recode(hospital, 
                  "hospital_1" = 1,
                  "hospital_2" = 2,
                  "hospital_3" = 3,
                  "hospital_4" = 4,
                  "hospital_5" = 5,
                  "hospital_6" = 6,
                  "hospital_7" = 7, 
                  "hospital_8" = 8,
                  "hospital_9" = 9, 
                  "hospital_10" = 10)
  )


#checking the data 

summary(AS3)

describe(AS3)

# seeing an error in the "sex", recoding it and rerunning the factor code

AS3$sex <- replace(AS3$sex, AS3$sex == "woman", "female")

AS3 = AS3 %>% 
  mutate(ID = factor(ID), sex = factor(sex))

#seeing an error in the household_income: one value is negative, correcting it 

AS3[order(AS3$household_income),] #we see its only one value so we can asjust it manually with the following code

AS3$household_income <- replace(AS3$household_income, AS3$household_income == "-7884", 7884)

# recoding it and checking again 

summary(AS3)



# building the model 



#ZK's custom function 

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}



#### CHECKING ASSUMPTIONS ####



#### Looking at the data and relationship ####

AS3 %>%
  ggplot() + aes(y = pain, x = age) + geom_point(aes(color = hospital),
                                                              size = 4) + geom_smooth(method = "lm", se = F)


int_plot = AS3 %>%
  ggplot() + aes(y = pain, x = age, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
int_plot

#very interesting! We have some hospitals, where it doesnt matter at all, and some where it seams to matter a lot! 



mod_as1 = lm( pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = AS3)

mod_as1 #model from assignment 1 



#making everything numeric so I can plot it lol 

AS3_num = AS3 %>%
  mutate(
    pain = as.numeric(pain), 
    age = as.numeric(age), 
    STAI_trait = as.numeric(STAI_trait), 
    pain_cat = as.numeric(pain_cat), 
    IQ = as.numeric(IQ), 
    sex = as.numeric(sex)
  )

# plot of the variables in the thingy 

AS3_num %>%
  ggplot() + aes(y = pain, x = sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness) + geom_point(aes(color = hospital),
                                                 size = 4) + geom_smooth(method = "lm", se = F)


int_plot2 = AS3_num %>%
  ggplot() + aes(y = pain, x = sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
int_plot2


#random intercept but no random slop 

int_plot2 + xlim(50, 200) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)


#random intercept AND random slope model 

slope_plot = AS3_num %>%
  ggplot() + aes(y = pain, x = sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE) + xlim(-1, 200) + geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)
slope_plot


#### model ####
mod_ri = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = AS3)
mod_ri

mod_fixed = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = AS3)
mod_fixed #model for comparison on AS3


#CIs etc 


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

sm = summary(mod_ri)
sm	

AIC(mod_ri)	

confint(mod_ri)

#comparison 

sum(residuals(mod_ri)^2)
sum(residuals(mod_fixed)^2)


cAIC(mod_ri)$caic		
cAIC(mod_fixed)$caic	

anova(mod_ri, mod_fixed)	




cAIC_fixed = round(cAIC(mod_fixed)$caic, 2)	
cAIC_ri = round(cAIC(mod_ri)$caic, 2)		
chisq = round(anova(mod_fixed, mod_ri)$Chisq[2], 2)		
chisq_p = round(anova(mod_fixed, mod_ri)$Pr[2], 3)		
chisq_df = anova(mod_fixed, mod_ri)[2,"Chi Df"]		
R2 = round(as.data.frame(r2beta(mod_ri, method = "nsj", data = AS3))[1,"Rsq"], 4)		
R2ub = round(as.data.frame(r2beta(mod_ri, method = "nsj", data = AS3))[1,"upper.CL"], 2)		
R2lb = round(as.data.frame(r2beta(mod_ri, method = "nsj", data = AS3))[1,"lower.CL"], 2)		


# In the results section, you should report the linear mixed models analysis with the following data: 	

# "The random slope model produced a better model fit both according to the likelihood ratio test ($\chi^2$ = `r chisq`, df = `r chisq_df`, p = `r substr(chisq_p, 2, nchar(chisq_p))`) and the cAIC (cAIC intercept = `r cAIC_int`, cAIC slope = `r cAIC_slope`). Thus, we present the results of the random slope model in the following. (The results of the random intercept model are listed in the supplement.)	

# The linear mixed model was significantly better than the null model, where the fixed effect predictor, weight, explained`r R2*100`% of the variance of sandwiches taken (R^2 = `r round(R2, 2)` [95% CI = `r R2lb`, `r R2ub`])."	

# You will also have to report statistics related to the predictors, this is usually done in a table format, because most often we have multiple predictors (even though in this example we only have one). You can get the information about the important results related to the predictors from the following functions:	

# Model coefficients and p-values:	


# The final table would look something like this:	



sm = summary(mod_ri)		
sm_p_values = as.character(round(sm$coefficients[,"Pr(>|t|)"], 3))		
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))		
sm_p_values[sm_p_values == "0"] = "<.001"		

coef_CI = suppressWarnings(confint(mod_ri))		

sm_table = cbind(as.data.frame(round(cbind(as.data.frame(sm$coefficients[,"Estimate"]), c(0, stdCoef.merMod(mod_ri)[1,7])), 2)), sm_p_values)
names(sm_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
sm_table["(Intercept)","Std.Beta"] = "0"		
sm_table		


summary(mod_ri)	


confint(mod_ri)	

stdCoef.merMod(mod_ri)	


#### model fixed #### 

mod_fixed

summary(mod_fixed)



#model 1

sm = summary(mod_fixed)	
sm	

AIC(mod_fixed)	

confint(mod_fixed)

lm.beta(mod_fixed)


sm_p_values = as.character(round(sm$coefficients[,4], 3))	
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))	
sm_p_values[sm_p_values == "0"] = "<.001"	


sm_table = cbind(as.data.frame(round(cbind(coef(mod_fixed), confint(mod_fixed), c(0, lm.beta(mod_fixed)$standardized.coefficients[c(2,3)])), 2)), sm_p_values)	
names(sm_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table["(Intercept)","Std.Beta"] = "0"	

# final coefficient table for model 1

sm_table = coef_table(mod_fixed)	
sm_table


# cAIC 

cAIC(mod_ri)$caic

cAIC(mod_fixed)$caic


### Variance explained #### 

# marginal R squared with confidence intervals	
r2beta(mod_ri, method = "nsj", data = AS3)	

# marginal and conditional R squared values	
r.squaredGLMM(mod_ri)	

r.squaredGLMM(mod_fixed)	

r2beta(mod_fixed, method = "nsj", data = AS3)





####### testing on new data ##############

# prediction of models on test set 

#matching AS4 the AS3

AS4 = AS4 %>% 
  mutate(ID = factor(ID), sex = factor(sex), hospital = factor(hospital))

AS4 = AS4 %>%
  mutate(pain = as.numeric(pain))

AS3 = AS3 %>%
  mutate(pain = as.numeric(pain))


AS4 = AS4 %>%
  mutate(
    hospital_nr = recode(hospital, 
                         "hospital_1" = 1,
                         "hospital_2" = 2,
                         "hospital_3" = 3,
                         "hospital_4" = 4,
                         "hospital_5" = 5,
                         "hospital_6" = 6,
                         "hospital_7" = 7, 
                         "hospital_8" = 8,
                         "hospital_9" = 9, 
                         "hospital_10" = 10)
  )



prediction = predict(mod_ri, AS4, allow.new.levels = T)

prediction

# sum of squares 

RSS_test = sum((AS4[,"pain"] - prediction)^2)	

RSS_test	

mod_ri_mean <- lm(pain ~ 1, data = AS3)

TSS_test = sum((AS4[,"pain"] - predict(mod_ri_mean))^2)

TSS_test

R2_predicted = 1 - (RSS_test/TSS_test)

R2_predicted


#### Building random slope model #### 

mod_slope = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), 
                 data = AS3)


AS3 = AS3 %>%
  mutate(pred_slope = predict(mod_slope))

AS3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
                                                       aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~ hospital, ncol = 2)
