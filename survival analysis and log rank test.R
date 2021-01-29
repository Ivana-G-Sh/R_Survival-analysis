#SURVIVAL ANALYSIS IN R FOR PUBLIC HEALTH
#What is Heart Failure and How to run a KM plot in R
setwd("~/Desktop/R_public_health")
g <- read.csv(file = "simulated HF mort data for GMPH (1K) final.csv", header=TRUE, sep=',')
dim(g)
g[1:5,]
#death (0/1)
#los (hospital length of stay in nights)
#age (in years)
#gender (1=male, 2=female)
#cabg (previous heart bypass)
#crt (cardiac resynchronisation device - a treatment for heart failure)
#defib (defibrillator implanted)
#ihd (ischaemic heart disease)
#mental_health (any mental illness)
#copd (chronic obstructive lung disease)
#pvd (peripheral vascular disease)
#prior_appts_attended (number of outpatient appointments attended in the previous year)
#prior_dnas (number of outpatient appointments missed in the previous year)
#pci (percutaneous coronary intervention)
#stroke (history of stroke)
#quintile (socio-economic status for patient's neighbourhood, from 1 (most affluent) to 5 (poorest))
#ethnicgroup (see below for categories 1=white  2=black 3=Indian subcontinent 8=not known  9=other)
#fu_time (follow-up time, i.e. time in days since admission to hospital)

library(survival)
#survfit
library(ggplot2)
#To run these packages, we of course need some variables to put into them. 
#My preferred way to do this is to turn each column of the data set, which we’ve called “g”, into a variable and tell R what kind of variable it is.
gender <- as.factor(g[,"gender"]) # R calls categorical variables factors
fu_time <- g[,"fu_time"] # follow up, continuous variable (numeric) 
death <- g[,"death"] # binary variable (numeric) 

#run an overall Kaplan-Meier plot
km_fit <- survfit(Surv(fu_time, death) ~ 1)
plot(km_fit)

#The “times” argument gives us control over what time periods we want to see. 
#The above code asks for output every day for the first week, then at 30, 60 and 90 days, and then every 90 days thereafter
summary(km_fit, times = c(1:7,30,60,90*(1:10))) 

#Now let’s extend this by splitting the curve by gender
km_gender_fit <- survfit(Surv(fu_time, death) ~ gender) 
plot(km_gender_fit)

#To compare survival by gender, we can run a logrank test.
survdiff(Surv(fu_time, death) ~ gender, rho=0) 
#p=0.8 Both genders seem to have similar survival rates over time or atleast there is no good evidence to contradict that

#I’d like you to compare survival by broad age group: those aged 65 and above versus those aged under 65
g$above65 <- ifelse(g$age >= 65, 1, 0)
g
km_age_fit <- survfit(Surv(fu_time, death) ~ g$above65) 
plot(km_age_fit)
#logrank
survdiff(Surv(fu_time, death) ~ g$above65, rho=0) 

#another way
age_65plus <- ifelse(g[,"age"]>=65,1,0) # dichotomise age
table(age_65plus, exclude = NULL) # inspect the numbers - always a good idea
age_65plus
age <- g[,"age"]
table(age,age_65plus, exclude = NULL)
survdiff(Surv(fu_time, death) ~ age_65plus, rho=0)
#p less than 0.01. Survival times do differ.
#But which group live longest after their hospital admission?
#You’d expect the younger group to live longest of course, but you don’t know that until you look at the above table. 
#The 115 younger patients (those with age_65plus = 0) had 18 observed deaths, but you would expect 67 under the null hypothesis of no difference in survival times by age group. 
#In contrast, the older group had more deaths than expected under the null, which confirms your instinct that younger patients live significantly (p<0.001, in fact very near zero) longer after hospital admission than older ones do.


#How to run Simple Cox model in R
library(survival)
g <- read.csv(file = "simulated HF mort data for GMPH (1K) final.csv", header=TRUE, sep=',')
#age as predictor, outcome is death
cox <- coxph(Surv(fu_time, death) ~age, data = g)
#to see the results
summary(cox)
#number of events i.e. patient outcomes - in this case deaths
#In a model where age is entered as just one term, age is assumed to have a linear relation with the hazard
#hazard ratio is 1.06  means that for each increase of one year in age the hazard of death at any given time point since the start of the study goes up by 6%
#se(coed) means standard error
#Pr(z) je zapravo p vrijednost - mala je pa R stavlja tri zvijezdice kraj nje
#lowe i upper se odnose na confidence interval

#ethnic group as a predictor
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup, data = g) # take variables straight from g
summary(cox)
#…you’ll get just one coefficient for ethnicgroup. 
#This is because unless you tell it otherwise, R will assume that all your variables are continuous. 
ethnicgroup <- factor(g[,"ethnicgroup"]) # can also use “as.factor” rather than “factor”
fu_time <- g[,"fu_time"]
death <- g[,"death"]

cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)

#Ethnicity is very obviously not a continuous variable, but R doesn’t know that unless you tell it!
#ethnicgroup (see below for categories 1=white  2=black 3=Indian subcontinent 8=not known  9=other)
summary(cox)
#43 observations were excluded because of missing data
#confidence interval for black people pretty wide:  0.5008-1.7558
#reference group is 1 i.e. white people
#black people =2, high p value,  black and white people appear to have the same hazar
#indians,  0.30, 95% CI 0.14 to 0.68, p=0.004. 
#That’s a statistically significant difference in favour of these patients compared with white patients

#assigning missing value i.e. NA to unknown category - will it change the cox results?
levels(ethnicgroup)<-c(levels(ethnicgroup),"8") # add level 8 to the factor
ethnicgroup[is.na(ethnicgroup)] <- "8" # Change NA to "None"
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup) 
summary(cox) 
#doesnt change the results much


#multiple Cox regression model
#Age (assumed to be continuous)
#Gender
#Prior OPD appointments missed (“prior_dnas”)
#Ethnic group
#COPD (chronic obstructive pulmonary disease)
# “summary” for continuous variables and “table” for categorical ones. 
#For “table”, you’ll need to remember to use the “exclude=NULL” option to display any missing or null values.
age <- g[,"age"]
summary(age)
hist(age)
#there’s a pretty wide spread of values and none is missing

gender <- as.factor(g[,"gender"])
table(gender, exclude=NULL)
# Also, note that both categories have lots of patients in.
#add proportions
t <- table(gender, exclude=NULL)
addmargins(t) # adds the total (a "sum" column)
round(100*prop.table(t),digits=1) # get %s rounded to 1dp
gender


levels(ethnicgroup)<-c(levels(ethnicgroup),"8") # add level 8 to the factor
ethnicgroup[is.na(ethnicgroup)] <- "8" # Change NA to "None"
table(ethnicgroup, exclude=NULL)

copd <- g[,"copd"]
table(copd, exclude=NULL)
t <- table(copd, exclude=NULL)
addmargins(t) # adds the total (a "sum" column)
round(100*prop.table(t),digits=1) # get %s rounded to 1dp
#24% of patients had COPD, with no missing values – though remember what I said in the earlier video about missing values masquerading as regular values. It’s actually likely that some patients have COPD but haven’t been recorded as having it. 
#Such underrecording of comorbidities is common with administrative data for various reasons.

prior_dnas <- g[,"prior_dnas"]
t <- table(prior_dnas, exclude=NULL)
addmargins(t)
round(100*prop.table(t),digits=1) # get %s rounded to 1dp 
#3/4 patients didnt skip the appointments, but nearly 3% missied 5 or more appointments
#Your options are:
#Pretend it’s continuous and assume a linear relation with the outcome
#Categorise it using each of the values as a category
#Categorise it but combine some values
#In general, the first option is preferred for ordinal variables with lots of values. You’ll need to check that the relation is linear, but if it is (more or less) linear, then this is best.
#Categorising it loses information. You’ll also run into practical difficulties if you try to fit a categorical variable with tiny numbers of patients in some of the categories, as you’ll see later.
#Having a few categories with lots of patients in each is, however, a good way of getting around the problem of a non-linear relation. In this case, as well, trying to assess whether the relation is linear is made harder by the sparse data for people with more than five or six missed appointments.



#run the model
#reference categories are white and males (this can be changed after)
#5 predictors (age, gender, copd, prior_dnas, ethnic group)
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + prior_dnas + ethnicgroup)
summary(cox)

#females are at lower risk (hazard ratio = 0.78, 95%ci 0.65-0.93, p = 0.007) of mortality than males following hospital admission for heart failure
#their hazard is 22% lower than that of the males (1-0.78)

#age, older age is a hazard - for every year of age hazard increases for 1.06 ie. 6%

#etnic group 3 (indians) have lower hazard (0.43, p= 0.05), they lived longer than the white people after their admission
#results of the Cox model ethnic group 9 (other) has an increased risk compared with ethnic group 1 (white) but the result is not statisti

#non-attendance is a statistically significant predictor of mortality.
#hazard ratio for the number of previous missed appointments which is 1.18 for each appointment missed for hazards increases by 18% that's quite a lot as with age the assumption here is that there is a linear relation between the predictor and the hazard for death

#so this Cox model showed statistically significant relations for age gender ethnic group - any just and missed appointments but not for COPD in this sample each of these relations is adjusted for the other predictors 


#Non-convergence
#quintile - quintile measures socio-economic status or deprivation
names(g)
quintile <- as.factor(g[,"quintile"])
table(quintile, exclude=NULL) 
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup) 
summary(cox)
#inf values...?
#qunitile cannot be 0, it is 0 because the zip code was invalid
#the second problem is that in the quitnile 0 group no one died - so it is not proper reference category
t <- table(quintile,death) 
t
round(100*prop.table(t,1),digits=1)

#option 1) change the reference category, change it ot quintile 1
quintile <- relevel(quintile, ref= "1")
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup) 
summary(cox)
#option 2) combine categories - in this case it is too small
quintile_5groups <- g[,"quintile"]
quintile_5groups[quintile_5groups==0] <- 5 # This picks the individuals with quintile=0 (note the double equals sign) and sets them to 5
quintile_5groups <- factor(quintile_5groups) # lastly, tell R that this is a categorical variable and not a continuous one
table(quintile_5groups, exclude=NULL) 
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile_5groups + ethnicgroup) 
summary(cox)
#option 3) exclude this patients
quintile_5groups <- g[,'quintile'] 
quintile_5groups[quintile_5groups==0] <- NA # set the zeroes to missing 
quintile_5groups <- factor(quintile_5groups) 
table(quintile_5groups, exclude=NULL) 
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile_5groups + ethnicgroup) 
summary(cox) 
#option 4) drop the entire variable
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + ethnicgroup) 
summary(cox)


#Checking the proportionality assumption for gender
fit <- coxph(Surv(fu_time, death) ~ gender) # fit the desired model
temp <- cox.zph(fit, transform="km", global=TRUE)# apply the cox.zph function to the desired model
print(temp) # display the results
plot(temp) # plot the curves p = 0.275

fit <- coxph(Surv(fu_time, death) ~ copd) # fit the desired model
temp <- cox.zph(fit, transform="km", global=TRUE)# apply the cox.zph function to the desired model
print(temp) # display the results
plot(temp) #p = 0.278

#What to do if the proportionality assumption is not met
##tt funstion
#ivide the survival analysis into two time periods. You can fit one model when things are fine, i.e. when the assumption is valid, and another model to cover the later follow-up period when the assumption is not valid.
#stratify the analysis by the variable that’s causing the problems. If it’s gender, for instance, then just fit separate models for males and females.


# make the other covariates 

ihd <- factor(g[,'ihd']) 

valvular <- factor(g[,'valvular_disease']) 

pvd <- factor(g[,'pvd']) 

stroke <- factor(g[,'stroke']) 

copd<- factor(g[,'copd'])

pneumonia <- factor(g[,'pneumonia']) 

ht <- factor(g[,'hypertension'])

renal <- factor(g[,'renal_disease']) 

ca <- factor(g[,'cancer']) 

mets <- factor(g[,'metastatic_cancer']) 

mental_health <- factor(g[,'mental_health']) 

los <- g[,'los']

prior_dna <- g[,'prior_dnas']

# generate cognitive impairment variable (senility and dementia combined)

cog_imp <- as.factor(ifelse(g$dementia == 1 | g$senile == 1, 1, 0))

# run the full model 

cox <- coxph(Surv(fu_time, death) ~ age + gender + ethnicgroup + ihd + 
               valvular + pvd + stroke + copd + pneumonia + ht + renal + 
               ca + mets + mental_health + cog_imp + los + prior_dna) 
#So only 79 patients had cognitive impairment, of whom 59 (74.7%) died. If you were doing logistic regression, you could convert this into an odds of death, giving 59 deaths divided by 20 non-deaths = 59/20=2.95. The impaired patients are three times more likely to die than they are to survive. 
#For those without such impairment, the risk of death is 47% and the odds are 433/488=0.89.
summary(cox) 
fit <- coxph(Surv(fu_time, death) ~ age + gender + valvular + pneumonia + 
               mets + cog_imp) # test them all in the same model 

temp <- cox.zph(fit)  

print(temp) 


#quiz
#the chi-square test for idependence - null hypothesis - in the population the two categorical variables are independent ==there is no relationship between the two categorical variables
survdiff(Surv(fu_time, death) ~ gender, rho=0)
#Chisq= 0.1  on 1 degrees of freedom, p= 0.8 We do not have have enough evidence to conclude that F and M variables are dependent