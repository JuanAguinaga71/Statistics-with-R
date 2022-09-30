#R_course

#seccion 2

data(cars)
head(cars)

plot(x=cars$speed, y=cars$dist)
cor.test(cars$speed, cars$dist)# pearson correlation
cor.test(cars$speed, cars$dist, method = "spearman", exact = FALSE)
cor.test(cars$speed, cars$dist, method = "kendal", exact = FALSE)

library(ppcor)
data(Cars93)
#Min.Price, MPG.city, MPG.highway
pcor.test(Cars93$MPG.city, Cars93$MPG.highway, Cars93$Min.Price, method = "pearson")

plot(Cars93$MPG.city, Cars93$Min.Price)
