library(tidyverse)

#MAKE ANALYTIC DATA 

#load data and replace missing values
my.data <- read_csv(file="exam_data_f16.csv", na=c("","NA","-999"))

glimpse(my.data)

#make categorical variables into factors 

categorical.variables <- select(my.data, gender, education)

categorical.variables$gender <- as.factor(categorical.variables$gender)

levels(categorical.variables$gender) <- list("Male"=1, "Female"=2)

###
categorical.variables$education <- as.factor(categorical.variables$education)

#create sets of scaled items (don't forget to rename this differently with _items)
glimpse(my.data)

agreeableness_items <- select(my.data, A1, A2, A3, A4, A5)

consc_items <- select(my.data, C1, C2, C3, C4, C5)

jobperf_items <- select(my.data, JP1, JP2, JP3, JP4, JP5)

age <- select(my.data, age)

#check its right for one - especially if you corrected an error 

View(consc_items)

#Check the scale of your items for out of range values, i.e., scale from 1 to 6 (nothing is 0 or 7+)

psych::describe(agreeableness_items)
#has a max value of 12 

psych::describe(consc_items)
#good 

psych::describe(jobperf_items)
#good

#correct out of range values

bad.value <- agreeableness_items <1 | agreeableness_items>6

agreeableness_items[bad.value] <- NA

psych::describe(agreeableness_items)

## FLIP REVERSE KEYED ITEMS with mutate 

####if scale is from 1 to 6 and item 1 is reverse keyed 
### 1 2 3 4 5 6
### 6 5 4 3 2 1 

agreeableness_items

agreeableness_items <- mutate (agreeableness_items, A1=7-A1)

agreeableness_items

###
consc_items

consc_items <- mutate (consc_items, C4=7-C4)
consc_items <- mutate (consc_items, C5=7-C5)

consc_items
###

jobperf_items <- mutate (jobperf_items, JP1=7-JP1)
jobperf_items <- mutate (jobperf_items, JP2=7-JP2)


## If items are made of scaled items, you need to get a single scaled score - remember to change the names (no item)

agreeable <- psych::alpha(as.data.frame(agreeableness_items), check.keys=FALSE)$scores

consc <- psych::alpha(as.data.frame(consc_items), check.keys=FALSE)$scores

jobperf <- psych::alpha(as.data.frame(jobperf_items), check.keys=FALSE)$scores

#combine everything (categorical variables and age too)

analytic_data <- cbind(categorical.variables, age, agreeable, consc, jobperf)

write_csv(analytic_data, path="exam_analytic_data.csv")

View(analytic_data)

library(apaTables)

apa.cor.table(analytic_data, filename="Exam_Table1_Correlation.doc", table.number = 1)

#check linearity
psych::pairs.panels(as.data.frame(analytic_data))

#CRONBACH

cron.data <- select(analytic_data, -gender, -education, - age)

psych::alpha(cron.data)

##MULTIPLE REGRESSION

glimpse(analytic_data)

#DV = jobperf
#IV1 = consc
#IV2 = agreeable

#how does conscientiousness alone predict job performance 

my.regression.baseline <- lm(jobperf ~ consc, analytic_data)

summary(my.regression.baseline)

apa.reg.table(my.regression.baseline)

my.regression <- lm(jobperf ~ consc + agreeable, analytic_data)

summary(my.regression)

apa.reg.table(my.regression)

apa.reg.table(my.regression, filename = "Exam_Reg_Table.doc", table.number=2)

#check linearity, homoscedasticity and normality 
library(ggplot2)

par(mfrow=c(2,2))
plot(my.regression)

#genders 
glimpse(analytic_data)

analytic.data.male <- filter(analytic_data, gender=="Male")
analytic.data.male <- select(analytic.data.male, - gender)

glimpse(analytic.data.male)


analytic.data.female <- filter(analytic_data, gender=="Female")
analytic.data.female <- select(analytic.data.female, -gender)

#regression for male 

my.regression.male <- lm(jobperf ~ consc + agreeable, analytic.data.male)

summary(my.regression.male)

apa.reg.table(my.regression.male)

apa.reg.table(my.regression.male, filename = "Exam_Reg_Male_Table.doc", table.number=3)

#regression for female 

my.regression.female <- lm(jobperf ~ consc + agreeable, analytic.data.female)

summary(my.regression.female)

apa.reg.table(my.regression.female)

apa.reg.table(my.regression.female, filename = "Exam_Reg_Female_Table.doc", table.number=4)
