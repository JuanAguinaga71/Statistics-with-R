#practice from the course
#Seccion 1 - test of association

#Exercise 1
#In the data file mobilenet.csv, determine whether there is a relationship
#between the number of hours spent on the Internet and the income, using
#the Pearson, Spearman and Kendall correlation.

mn<- read.csv("csv-data-frames/mobilenet.csv")

Pearson_test<- cor.test(mn$hours, mn$income, method = "pearson")
# Pearson_test: it is rejected null hipotesisp <0.5. 

Spearman_test<- cor.test(mn$hours, mn$income, method = "spearman", exact = F)
# Spearman_test: it is rejected null hipotesis p<0.5. 

Kendall_test<- cor.test(mn$hours, mn$income, method = "kendall", exact = F)
# Kendall_test: it is rejected null hipotesis p<0.5.


#Exercise #2
#In the data file countries.csv, determine whether there is a relationship
#between the percentage of people living in cities (urban) and the gross
#domestic product (gdp), using the Pearson, Spearman and Kendall
#correlation.

countries<- read.csv("csv-data-frames/countries.csv")
     
Pearson_test_countr<- cor.test(countries$urban, countries$gdp, method = "pearson")
# Pearson_test_countr: it is rejected null hipotesis p<0.5. 

Spearman_test_countr<- cor.test(countries$urban, countries$gdp, method = "spearman", exact = F)
# Spearman_test_countr: it is rejected null hipotesis p<0.5. 

Kendall_test_countr<- cor.test(countries$urban, countries$gdp, method = "kendall", exact = F)
# Kendall_test_countr: it is rejected null hipotesis p<0.5.

#Exercise #3
#In the data file countries.csv, compute the correlation between the
#percentage of people living in cities (urban) and the gross domestic product
#(gdp), corrected with the influence of the percentage of people who read
#(literacy).

install.packages("ppcor")
library(ppcor)

any(is.na(countries))
sum(is.na(countries))
countries2<-countries[complete.cases(countries),]#for the test necessary to remove NAS 

PartialCor_test_countr<- pcor.test(countries2$urban, countries2$gdp,
                                   countries2$literacy, method = "pearson")
# PartialCor_test_countr: it is rejected null hipotesis p<0.5.


#Exercise #4
#In the data file directmail.csv, run a chi-square test for association to
#determine whether there is a relationship between income and education
#level (educ).
dm<- read.csv("csv-data-frames/directmail.csv")
install.packages(gmodels)
library(gmodels)

ChiSquare_test_dm<- CrossTable(dm$income, dm$educ, expected=T, prop.r=F,
                               prop.c=F, prop.t=F, prop.chisq=F)
#ChiSquare_test_dm: there is no relationship between income and education p=0.64