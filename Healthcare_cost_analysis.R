#read Healthcare cost analysis data from file
hospital<- read.csv(file.choose())
head(hospital)

# check summary
summary(hospital)
#attach data for easy to access
attach(hospital)

#Start Analysis 1 -------->#
#1. To record the patient statistics, the agency wants to find the age category of people who frequent the hospital and has the maximum expenditure. 
table(AGE)
colors = c("red", "yellow", "green", "violet", "orange","blue", "pink", "cyan") 
hist(AGE, col=colors)
#to see the value of category of given age group
ag<-as.factor(AGE)
summary(ag)
#age category of 0 is more frequently using the hospital
tapply(TOTCHG,AGE,sum)
#max expenditure ia also age group of 0 age =678118
which.max(tapply(TOTCHG,AGE,sum))
#End Analysis 1 -------->#


#Start Analysis 2 -------->#
#2. In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure.
diagg<-as.factor(APRDRG)
summary(diagg)
# APRDRG category 640 has the maximum entries of hospitalization
which.max(summary(diagg))
tapply(TOTCHG,diagg,sum)
which.max(tapply(TOTCHG,diagg,sum))
# also highest total hospitalization cost (437978)
max(tapply(TOTCHG,diagg,sum))
#End Analysis 2 -------->#

#Start Analysis 3 -------->#
# 3. To make sure that there is no malpractice, the agency needs to analyse if the race of the patient is related to the hospitalization costs.
#H0:The race of the patient is related to the hospitalization costs. 
#Ha:The race of the patient is NOT related to the hospitalization costs.
# create anova model with Hospital discharge costs (TOTCHG) over Race of the patient 
model_host_annova<-aov(TOTCHG~RACE)
summary(model_host_annova)
#pvalue above is 68% which is very high and to conside RACE as cost factor could be a risk so we can reject the null hypothesis
#this means -> The race of the patient is NOT related to the hospitalization costs. 
#End Analysis 3 -------->#


#Start Analysis 4 -------->#
#4. To properly utilize the costs, the agency has to analyse the severity of the hospital costs by age and gender for the proper allocation of resources.
#simple regression linear model to analyze cost over age and gender
model_host_lm_m1<-lm(TOTCHG~AGE+FEMALE)
summary(model_host_lm_m1)
# if you check the output  p value of age is very less so we can say age is very important facor for hospital cost 
# Gebder is also important facotr 
# F-statistic:  6.66 on 2 and 497 DF,  p-value: 0.001399
#End Analysis 4 -------->#


#Start Analysis 5 -------->#
#5. Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.
#simple regression linear model to analyze crucial factor for inpatients length of stay (LOS) -> over age , gender and RACE
model_host_lm_m2<-lm(LOS~AGE+FEMALE+RACE)
summary(model_host_lm_m2)
#The very high p-value means there is no linear relationship between AGE, Gender and  RACE.
#So it seems only with age, gender, and race of the patient it is not possible to predict the LOS
#End Analysis 5 -------->#

#Start Analysis 6 -------->#
#6. To perform a complete analysis, the agency wants to find the variable that mainly affects hospital costs.
#Remove if any NA value
hospital_na<-na.omit(hospital)
# create model with all variables on TOTCHG
model_host_lm_m3<-lm(TOTCHG~ .,data=hospital_na) 
summary(model_host_lm_m3)
#If we analyze the summary we can see age and length of stay affect mainly the hospital cost
#APRDRG also affect the affects significantly the hospital costs
#End Analysis 6 -------->#

