# library
library(Hmisc)

###############
## DATA
###############
####################
### NHANES DATA (2017-2018)
####################
MI_data <- sasxport.get("MCQ_J.XPT")
head(MI_data)
demo_data <- sasxport.get("DEMO_J.XPT")
head(demo_data)
smoking_data <- sasxport.get("SMQ_J.XPT")
head(smoking_data)
alcohol_data <- sasxport.get("ALQ_J.XPT")
head(alcohol_data)
BMI_data <- sasxport.get("BMX_J.XPT")
head(BMI_data)
diabetes_data <- sasxport.get("DIQ_J.XPT")
head(diabetes_data)
BP_data <- sasxport.get("BPX_J.XPT")
head(BP_data)
HDL_data<-sasxport.get("HDL_J.XPT")
head(HDL_data)
LDLD_data<-sasxport.get("TRIGLY_J.XPT")
chol_data <- sasxport.get("TCHOL_J.XPT")
head(chol_data)
# depression
Depression_data<-sasxport.get("DLQ_J.XPT")
# Physical activity
Physical_data <-sasxport.get("PAQ_J.XPT")


#### Merge data
nhanes<-merge(MI_data, diabetes_data, by="seqn")
df_list<-list(nhanes, demo_data, smoking_data, alcohol_data, BMI_data,BP_data, 
              HDL_data,LDLD_data,chol_data,Depression_data,Physical_data)
datum<-Reduce(function(x,y) merge(x,y), df_list)


### Identify variables for analysis
colnames(datum)
datum_17_18<-datum[,c(1,26,77,130,132:133,135,139,145:146,168:174,175,177,213,
                      229,248:249,251:252,254:255,257:258,260,263,265,271,282:283)]
colnames(datum_17_18)



####################
### NHANES DATA (2015-2016)
####################
MI_data2 <- sasxport.get("MCQ_I.XPT")
head(MI_data2)
demo_data2 <- sasxport.get("DEMO_I.XPT")
head(demo_data2)
smoking_data2 <- sasxport.get("SMQ_I.XPT")
smoking_data2
alcohol_data2 <- sasxport.get("ALQ_I.XPT")
alcohol_data2
BMI_data2 <- sasxport.get("BMX_I.XPT")
BMI_data2
diabetes_data2 <- sasxport.get("DIQ_I.XPT")
diabetes_data2
BP_data2 <- sasxport.get("BPX_I.XPT")
BP_data2
HDL_data2<-sasxport.get("HDL_I.XPT")
head(HDL_data2)
LDLD_data2<-sasxport.get("TRIGLY_I.XPT")
chol_data2 <- sasxport.get("TCHOL_I.XPT")
chol_data2 
# depression
Depression_data2<-sasxport.get("DLQ_I.XPT")
# Physical activity
Physical_data2 <-sasxport.get("PAQ_I.XPT")


# Merge data
nhanes2<-merge(MI_data2, diabetes_data2, by="seqn")
df_list2<-list(nhanes2, demo_data2, smoking_data2, alcohol_data2, BMI_data2, 
               BP_data2, HDL_data2,LDLD_data2,chol_data2,Depression_data2,
               Physical_data2)
datum2<-Reduce(function(x,y) merge(x,y), df_list2)

### Identify variables for analysis
colnames(datum2)
datum_15_16<-datum2[,c(1,25,91,144,146:147,149,153,159:160,183:189,190,192,235,
                       249,273:274,276:277,279:280,282:283,285,288,290,292,
                       303:304)]
colnames(datum_15_16)


####################
# Append data (2015-2017)
####################
complete_data<-rbind(datum_15_16, datum_17_18)
colnames(complete_data)


###################
# DATA CLEANING
####################
##################################
# RECODE VARIABLES
##################################
### Myocardial infarction
table(complete_data$mcq160e)
complete_data$mcq160e[complete_data$mcq160e==9] <- NA
complete_data$MI <- factor(complete_data$mcq160e, levels=c(1,2), 
                           labels=c("Yes","No"), ordered=FALSE)
complete_data$MI <- relevel(complete_data$MI, ref="No")
table(complete_data$MI)

##### Diabetes
table(complete_data$diq010)
complete_data$diq010[complete_data$diq010 %in% c(7,9)] <- NA
complete_data$diabetes <- factor(complete_data$diq010, levels=c(1,2,3), 
                                 labels=c("Yes","No","Borderline"), 
                                 ordered=FALSE)
complete_data$diabetes <- relevel(complete_data$diabetes, ref="No")
table(complete_data$diabetes)

##### Age
summary(complete_data$ridageyr)
sum(is.na(complete_data$ridageyr))

##### Gender
# 1: male; 2: Female
table(complete_data$riagendr)
sum(is.na(complete_data$riagendr))

##### Ethnicity
table(complete_data$ridreth1)
complete_data$race <- factor(complete_data$ridreth1, levels=1:5, 
                             labels=c("Mexican American", "Other Hispanic", 
                                      "White", "Black", "Other"), ordered=FALSE)
complete_data$race <- relevel(complete_data$race, ref="White")
table(complete_data$race)

##### Marital status
# 1: never married; 2:married & living with partner; 
# 3:widowed/divorced/seperated
table(complete_data$dmdmartl)
complete_data$dmdmartl[complete_data$dmdmartl==77] <- NA
complete_data<-complete_data %>% 
  mutate(marital=ifelse(dmdmartl==1|dmdmartl==6,2,
                        ifelse(dmdmartl==2|dmdmartl==3|dmdmartl==4,3,
                               ifelse(dmdmartl==5,1,NA))))
table(complete_data$marital)

##### Education
# 1: less than high school; 2: high school; 3: more tha high school
table(complete_data$dmdeduc2)
complete_data$dmdeduc2[complete_data$dmdeduc2==9] <- NA
complete_data<-complete_data %>% 
  mutate(education=ifelse(dmdeduc2==1|dmdeduc2==2,1,
                          ifelse(dmdeduc2==3,2,
                                 ifelse(dmdeduc2==4|dmdeduc2==5,3,NA))))
table(complete_data$education)


##### Family poverty ratio
summary(complete_data$indfmpir)
complete_data$fam_inc_pov<-ifelse(complete_data$indfmpir<1.5,0,
                                  ifelse(complete_data$indfmpir>=1.5 & complete_data$indfmpir<3.5,1,
                                         ifelse(complete_data$indfmpir>=3.5,2,complete_data$indfmpir)))
table(complete_data$fam_inc_pov) # 1732 1660 1339 
factor(complete_data$fam_inc_pov)


### SMOKING
## temporarily assign the SmokeCigs variable as individuals' 
## response to SMQ040
complete_data$smoking <-complete_data$smq040

## re-codes individuals who responded "No" to "Ever smoked 100 cigarettes in your life" (SMQ020) 
## as -999 instead of missing (since these individuals were never asked question SMQ040)
complete_data$smoking[complete_data$smq020 == 2] <- -999

## re-code individuals who answered "some days" to SMQ040 ("do you now smoke cigarettes")
## as "1". These individuals are considered current smokers by our definition
complete_data$smoking[complete_data$smoking == 2] <- 1

## finally, create the factor variable based on our re-coding 
complete_data$smoking <- factor(complete_data$smoking, levels=c(-999, 3, 1), labels=c("Never", "Former", "Current"), ordered=FALSE)

table(complete_data$smoking) #  3128    1241    1007 

##### ALCOHOL
summary(complete_data$alq130)
complete_data$alq130[complete_data$alq130==999] <- NA


##### HYPERTENSION
#BLOOD PRESSURE
colnames(complete_data)
# systolic
complete_data$systolic<-rowMeans(complete_data[,c(22,24,26,28)], na.rm = T)
# CHECK- sys
colnames(complete_data)
w<-complete_data[,c(22,24,26,28,43)]

# diastolic
complete_data$diastolic<-rowMeans(complete_data[,c(23,25,27,29)], na.rm = T)
# CHECK- sys
colnames(complete_data)
m<-complete_data[,c(23,25,27,29,44)]

#### HYPERTENSION
complete_data$hypertension<-ifelse(complete_data$systolic>=140 |complete_data$diastolic>=90, 1,
                                   ifelse(complete_data$systolic<140&complete_data$diastolic<90, 0,NA))
table(complete_data$hypertension) #14057 1117 
factor(complete_data$hypertension)

##### BMI
summary(complete_data$bmxbmi)
complete_data$BMI <-cut(complete_data$bmxbmi, breaks=c(0, 18.5, 25, 30, Inf),
                        labels=c("Underweight","Normal","Overweight","Obese"))
complete_data$BMI <- relevel(complete_data$BMI, ref="Normal")
table(complete_data$BMI)

##### HDL
summary(complete_data$lbdhdd)
complete_data$HDL<-complete_data$lbdhdd

##### Triglyceride
summary(complete_data$lbxtr)
complete_data$triglyceride<-complete_data$lbxtr

##### Total chol
summary(complete_data$lbxtc)
complete_data$tchol<-complete_data$lbxtc


########################
# Identify main variables
##########################
colnames(complete_data)
main_data<-complete_data[,c(1,36:37,5,6,20,38:49,4,11:14)]
colnames(main_data)


###################
# Missing data
####################
colSums(is.na(complete_data))
### remove missing data
main_data2<-na.omit(main_data)



####################
## Check for multicolinearity
####################
colnames(main_data)
model<-glm(MI~diabetes+riagendr+ridageyr+race+marital+alq130+education+
             smoking+hypertension+fam_inc_pov+BMI+tchol+triglyceride+HDL,
           data=main_data2, family = binomial)
summary(model)
sump<-exp(cbind(OR=coef(model), confint(model))) 
round(sump, digits = 2)

# VIF
library(car)
vif(model)


####################
##### Hosmer-Lemeshow goodness-of-fit test 
####################
#install.packages("glmtoolbox")
library(glmtoolbox)

hltest(model)


####################
## DATA VISUALIZATION
####################
colnames(main_data2)
# Age
a<-ggplot(main_data2, aes(ridageyr))+
  geom_histogram(binwidth=5, fill="blue")+
  theme_classic()
# Total cholesterol
b<-ggplot(main_data2, aes(tchol))+
  geom_histogram(binwidth=15, fill="red")+
  theme_classic()
# HDL
c<-ggplot(main_data2, aes(HDL))+
  geom_histogram(binwidth=15, fill="green")+
  theme_classic()
# Triglyceride
d<-ggplot(main_data2, aes(triglyceride))+
  geom_histogram(binwidth=15, fill="yellow") +
  theme_classic()


## GRID
gridExtra::grid.arrange(a,b,c,d)





################################################################
# SURVEY DATA ANALYSIS  
################################################################
library(survey)
main_data$mec4yr= 1/2*main_data$wtmec2yr
nhc <- svydesign(id=~sdmvpsu, weights=~mec4yr, strata=~sdmvstra, 
                 nest=TRUE, survey.lonely.psu = "adjust", data=main_data)
nhc

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")


##############
### TABLE ONE
###############
library(tableone)
tab2 <- svyCreateTableOne(vars = c("ridageyr","riagendr","race","marital",
                                   "education", "BMI","smoking","alq130",
                                   "diabetes","hypertension","tchol",
                                   "fam_inc_pov","triglyceride", "HDL"), 
                          strata = "MI", data = nhc, 
                          factorVars = c("riagendr","race","marital",
                                         "education", "BMI","smoking",
                                         "diabetes","hypertension",
                                         "fam_inc_pov","MI"))

## Detailed output
summary(tab2)




####################
## STEPWISE ELIMINATION
####################
library(MASS)
# Stepwise regression model
step.model <- stepAIC(model, direction = "both", 
                      trace = FALSE)
summary(step.model)



#################
# LOGISTIC REGRESSION (SURVEY)
################
logit <- (svyglm(MI~factor(diabetes)+factor(riagendr)+ridageyr+factor(race)+
                   factor(marital)+factor(education)+factor(smoking)+
                   factor(hypertension)+factor(BMI)+ tchol+HDL, 
                 family=binomial, design=nhc, na.action = na.omit))
summary(logit)
# OR
sump<-exp(cbind(OR=coef(logit), confint(logit))) 
round(sump, digits = 2)


