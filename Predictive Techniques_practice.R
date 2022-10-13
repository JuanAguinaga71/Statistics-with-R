#practice from the course
#Section 3 - Predictive Techniques

# Exercise #1
# Using the data set vehicles.csv, run a regression analysis to determine the
# influence of the variables engine, horsepow, wheelbas, width, length,
# weight fuelcap and type on the variable price.
veh<- read.csv('csv-data-frames/vehicles.csv')
# View(veh)
# any(is.na(veh))
# sum(is.na(veh))
veh<- veh[complete.cases(veh),]

fit0 <- lm(price~engine+horsepow+wheelbas+width+length+fuelcap, data = veh)
fit1<- lm(price~engine, data = veh)
fit2<- lm(price~horsepow, data = veh)
fit3<- lm(price~wheelbas, data = veh)
fit4<- lm(price~width, data = veh)
fit5<- lm(price~length, data = veh)
fit6<- lm(price~weight, data = veh)
fit7<- lm(price~fuelcap, data = veh)
summary(fit0)
summary(fit1) #sig
summary(fit2) #sig
summary(fit3) #sig
summary(fit4) #ns
summary(fit5) #ns
summary(fit6) #sig
summary(fit7) #sig

#for cheking outliers (was checked in the console from ou1 to ou7 )
res <- residuals(fit7) 
zres7 <- scale(res)
#View(zres7)
sum(zres7>3 | zres7< -3)
outs7<- (zres7>3 | zres7< -3)
ou7<-which(outs7)
outList<- c(ou1,ou2, ou3, ou4, ou5, ou6, ou7)
allOuts<-unique(outList)


# Exercise #2
# Perform a sequential regression with the variables described in the
# previous exercise, entering them in the regression model as follows:
#       - first block: engine, horsepow, type
#      - second block: the remaining variables.
veh1<- veh[-allOuts,]

fit1st<- lm(price~engine+horsepow+type, data = veh1)
fit2st<- lm(price~engine+wheelbas+width+length+fuelcap+mpg, data = veh1)

summary(fit1st)
summary(fit2st)

anova(fit1st, fit2st)


# Exercise #3
# Using the data set bankloan.csv, run a regression analysis to determine
# whether the customer balance depends on the following variable:
#       - age
bl<- read.csv('csv-data-frames/bankloan.csv')
View(bl)

fit<- lm(balance~age, data = bl)
summary(fit)
# - whether they have a loan or not
fit<- lm(balance~age+loan, data = bl)
summary(fit)

#there is stat. diff between levels

