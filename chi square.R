#chi square

bf<- read.csv("C:/Users/Dell/Dropbox/R_github/Statistics with R - Intermediate Level/csv-data-frames/breakfast.csv")

head(bf, 10)
str(bf)


install.packages("gmodels")
library(gmodels)

chiSquare_Test<- CrossTable(bf$agecat, bf$bfast, expected = T, prop.t = F, prop.c = F,
           prop.chisq = F)
sum(chi_results$prop.row[2,1:3])

fisher_Test<-fisher.test(bf$agecat,bf$bfast, simulate.p.value = T)


install.packages("lsr")
library(lsr)

cramersV_Test<- cramersV(bf$agecat, bf$bfast)
#############
#TWO WAY ANOVA - basic and simple main effects

vit<-read.csv("csv-data-frames/vitamin2.csv")
head(vit)
#basic
aov0<- aov(effort~dose+gender+dose*gender, data = vit)
summary(aov0)
TukeyHSD(aov0)

#simple
vit1<- vit[vit$gender=="female",]
View(vit1)
aov1<- aov(effort~dose, data = vit1)
summary(aov1)
TukeyHSD(aov1)
#The same with the other single parameters (male, d1,d2,d3)


#THREE WAY ANOVA - basic, simple interactions, simple main effects

#basic
vit<-read.csv("csv-data-frames/vitamin3.csv")
head(vit)
unique(vit$type)
aov0<- aov(effort~dose+gender+type+dose*gender+dose*type+gender*type
           +dose*gender*type, data = vit)
summary(aov0)

#simple interactions
vitm<- vit[vit$gender=="male", ]
aovm<- aov(effort~dose*type, data = vitm)
summary(aovm)

vitf<- vit[vit$gender=="female",]
aovg<- aov(effort~dose*type, data = vitf)
summary(aovg)

#simple main effects

#vitfw<- vitf[vitf$type=="white collar",]
vitfw<- vit[vit$gender=="female" & vit$type=="white collar",]

aovmw<- aov(effort~dose, data = vitfw)
summary(aovmw)
TukeyHSD(aovmw)
#The same with the other single parameters

#ONE WAY MANOVA

vit<-read.csv("csv-data-frames/vitamin-m.csv")

install.packages("biotools")
library(biotools)

vit2<- vit[c(1,2)]

boxM(vit2,vit$dose)# for testing covariance

#create matrices for independent and dependent variables
y<- cbind(vit$effort, vit$stress)
head(y)
dose<- vit$dose
~dose

manova1<-manova(y~dose) 
summary(manova1,test = "Wilks") #when iqual variances
summary(manova1, test = "Pillai")
# tukey test must be done one dependet variable at the time
aov1<- aov(effort~dose, vit)
TukeyHSD(aov1)
aov2<- aov(stress~dose, vit)
TukeyHSD(aov2)


#NON PARAMETRIC TEST( Mann whitney test)
sp<- read.csv("csv-data-frames/spanish.csv")
tail(sp, 20)

wilcox.test(sp$score~sp$course)
wilcox.test(sp$score~sp$course, correct=F)

install.packages("psych")
library(psych)
describeBy(sp$score, sp$course)

#wilcoxon test (when it is paired)
m<-read.csv("csv-data-frames/math.csv")
head(m)
wilcox.test(m$grade1,m$grade2, paired = T)
median(m$grade2)

wilcox.test(m$grade1,m$grade2, paired = T, correct = F)

#kruskal-wallis test
vit<- read.csv("csv-data-frames/vitamin1.csv")
head(vit)
unique(vit$dose)

kruskal.test(vit$effort~vit$dose)

library(psych)
describeBy(vit$effort,vit$dose)


