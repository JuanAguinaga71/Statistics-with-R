#R_course

#seccion 2


# this section is related to correlation
data(cars)
head(cars)

plot(x=cars$speed, y=cars$dist)
#Pearson correlation
cor.test(cars$speed, cars$dist)# pearson correlation
#Spearman correlation
cor.test(cars$speed, cars$dist, method = "spearman", exact = FALSE)
#Kendall correlation
cor.test(cars$speed, cars$dist, method = "kendal", exact = FALSE)
#Partial correlation
library(ppcor)
data(Cars93)
pcor.test(Cars93$MPG.city, Cars93$MPG.highway, Cars93$Min.Price, method = "pearson")
plot(Cars93$MPG.city, Cars93$Min.Price)

#Chi-square test
install.packages("gmodels")
library(gmodels) 

data("women")
head(women)
rm(cats)

stud<- read.csv('csv-data-frames/students.csv')
head(stud)
fit<-lm(score~iq+hours, data = stud)
summary(fit)
plot(lm)

#check the outliers
res<- residuals(fit)
zres<- scale(res)
View(zres)

#independence of errors
install.packages('car')
require(car)
durbinWatsonTest(fit)

#to check multicolinearity

x<- data.frame(stud$iq, stud$hours)
View(x)
install.packages('usdm')
require(usdm)
vif(x)

#homoschedasticity
require(ggplot2)
pred<- fitted(fit)
dat<-  data.frame(pred, res)
View(dat)
ggplot()+geom_point(data = dat, aes(x= res, y = pred))

#normality of residuals
shapiro.test(res)

#section 4

#cronbach's alpha
brd<- read.csv('csv-data-frames/brand.csv')

install.packages('psy')
require(psy)
cronbach(brd)

#cohen's kappa 
tch<- read.csv('csv-data-frames/teachers.csv')
install.packages("fmsb")
library(fmsb)
Kappa.test(tch$teacher1,tch$teacher2)

#kendall's w 
gym<- read.csv('csv-data-frames/gym.csv')
install.packages('irr')
require(irr)
kendall(gym)
