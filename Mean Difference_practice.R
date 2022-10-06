#practice from the course
#Seccion 2 - Mean difference 

#Exercise #1
#In the students.csv data frame, check whether there is a difference in
#average score between male and female students. (In the variable gender,
#the male students are coded with 1 and the female students with 0).

st<- read.csv("csv-data-frames/students.csv")
#str(st)
#head(st)
library(car)
leveneTest(st$score, st$gender) #there is no homogeneity of variances
t.test(st$score~st$gender, var.equal=F)
#Welch Two Sample t-test:it is rejected null hipotesis p<0.05. 
#true difference in means between groups

#Exercise #2
#In the bankloan.csv data set, determine whether there is a difference in
#average balance between people who have a loan (loan=”yes”) and people
#who do not (loan=”no”).

bl<- read.csv("csv-data-frames/bankloan.csv")
#str(bl)
#head(bl,10)
#sum(complete.cases(bl))
#any(is.na(bl))
leveneTest(bl$balance,bl$loan)#there is no homogeneity of variances
t.test(bl$balance~bl$loan, var.equal=F)
#Welch Two Sample t-test:it is rejected null hipotesis p<0.05. 
#true difference in means between groups


#Exercise #3
#In the data set gym.csv , determine whether there is a difference between
#the average score awarded by the judges from Romania and United States.

gym<-read.csv("csv-data-frames/gym.csv")
#str(gym)
#head(gym)
t.test(gym$romania, gym$unitedstates, paired = T)
#Paired t-test:it is rejected null hypothesis p<0.05. 
#true difference in means between groups


#Exercise #4
#In the data file incomedata.csv, determine whether the subjects’ income is
#influenced by their education level, using the one-way analysis of variance.

incd<- read.csv("csv-data-frames/incomedata.csv")
#head(incd)
#any(is.na(incd))
library(car)
leveneTest(incd$income, incd$educ)#there is no homogeneity of variances
aov1<- oneway.test(income~educ, data = incd, var.equal = F)
aov1 
install.packages("rstatix")
library(rstatix)
games_howell_test(income~educ, data = incd , conf.level = 0.95, detailed = FALSE)
#one-way analysis of variance:it is rejected null hypothesis p<0.05. 
#true difference in means between groups

#Exercise #5
#In the data file incomedata.csv, determine whether the subjects’ income is
#influenced by their education level and age, using the two-way analysis of
#variance.

incd<- read.csv("csv-data-frames/incomedata.csv")
#head(incd)
aov1<- oneway.test(income~educ+agecat+educ*agecat, data = incd, var.equal = F)
aov1
#one-way analysis of variance:it is rejected null hypothesis p<0.05. 
#true difference in means between groups


#####################################################################
#simple main effects
unique(incd$educ) 
head(incd)
incd_ed_sc<- incd[incd$educ=="Some college",]
aov1 = aov(income~agecat, data=incd_ed_sc)
summary(aov1)
##
#incd_educ<-list()
#for (i in incd$educ){
#      #i=1
#      incd_educ[i]<- incd[incd$educ[i],]
#      aov1 = aov(income~agecat, data=incd_educ[1])
#      aov_results(summary(aov1))
#      print(aov_results)
#}
###################################################################

#Exercise #6
#In the data file incomedata.csv, determine whether the subjects’ income is
#influenced by their education level, age and gender, using the three-way
#analysis of variance.
incd<- read.csv("csv-data-frames/incomedata.csv")
head(incd)

aov1<- oneway.test(income~educ+agecat+gender+
                         educ*agecat+
                         educ*gender+
                         gender*agecat+
                         educ*agecat*gender, data = incd, var.equal = F)
#It is used "oneway.test" when there is no homogeneity of variances; 
#however,it seems it does not support Threeway ANOVA.

#Exercise #7
#In the data frame vehicles.csv, determine whether the variables price and
#sale are influenced by the vehicle type (automobile or truck), using the
#multivariate analysis of variance.

veh<- read.csv("csv-data-frames/vehicles.csv")
#head(veh)
a#ny(is.na(veh))
#veh<-veh[complete.cases(veh),]# only selected complete cases(no NAs)for analysis 
#any(is.na(veh))
library(biotools)
veh2<-veh[c(3,1)]
boxM(veh2, veh$type)# There is no homogeneity of covariance p<0.05
dep_var <- cbind(veh$price, veh$sales)
indep_var<- veh$type
model<- manova(dep_var~indep_var)
summary(model, test = "Pillai") #Pillai model when non homogeneity of covariance
#manova:it is rejected null hypothesis p<0.05. 
#true difference in means between groups

#Exercise #8
#In the data set satisfaction.csv, check whether there is a difference
#between the median satisfaction of the two types of travelers (business
#and pleasure) using the Mann-Whitney test.
sat<- read.csv("csv-data-frames/satisfaction.csv")
View(sat)
getwd()
wilcox.test(sat$satisfaction~sat$type)
wilcox.test(sat$satisfaction~sat$type, correct=F)
#H0 it is rejected null hypothesis p<0.05. 
#true difference in means between groups
require(psych)
describeBy(sat$satisfaction, sat$type)

#Exercise #9
#In the data set teachers.csv, check whether there is a difference between
#the median grades of the two teachers using the Wilcoxon test.

teach<- read.csv("csv-data-frames/teachers.csv")
View(teach)
wilcox.test(teach$teacher1, teach$teacher2, paired = T)
wilcox.test(teach$teacher1, teach$teacher2, paired = T, correct = F)
median(teach$teacher1)
median(teach$teacher2)

#Exercise #10
#In the data set satisfaction.csv, check whether there is a difference
#between the median satisfaction of the customer groups by price
#importance using the Kruskal-Wallis test.

sat<- read.csv("csv-data-frames/satisfaction.csv")
head(sat)
kruskal.test(sat$imprice, sat$type)
#it is acepted null hypothesis p=0.3177. 
#there is no true difference in means between groups
require(psych)
describeBy(sat$imprice, sat$type)# get medians
