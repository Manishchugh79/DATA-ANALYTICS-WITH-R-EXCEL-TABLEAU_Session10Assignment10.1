install.packages('iterators')
library(iterators)
con <- bzfile('airquality.csv', 'r')
airqualitytest <- read.csv(unzip("C:/Users/Manish Chugh/Desktop/ACADGILD/Excel files/airquality.zip", files = NULL, list = FALSE, 
                        overwrite = TRUE, junkpaths = FALSE, exdir = ".", 
                        unzip = "internal",setTimes = FALSE))
airqualitytest

summary(airquality)

apply(airquality, 2, mean, na.rm=T)

airquality$Ozone = numeric(airquality$Ozone)
airquality$Ozone
summary(airquality$Ozone)

par(mfrow=c(2,3))
hist(airquality$Ozone, prob=T)
hist(airquality$Solar.R, prob=T)
hist(airquality$Wind, prob=T)
hist(airquality$Temp, prob=T)
hist(airquality$Month, prob=T)
hist(airquality$Day, prob=T)


pairs(airquality[,1:7])

library(purrr)
library(tidyr)
library(ggplot2)

airquality %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key,scales = "free") +
  geom_histogram()

# Check for missing values in all columns
# use summary
summary(airquality)
apply(airquality, 2, mean, na.rm=T)
summary(airquality$Ozone)
summary(airquality$Solar.R)

# Impute the missing values using appropriate methods
apply(airquality, 2, mean, na.rm=T)
# check file structure
str(airquality)
# apply library MICE
library(mice)
md.pattern(airquality)

# visualizing
install.packages("VIM")
library(VIM)
mice_plot <- aggr(airquality, col=c('green','blue'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(airquality), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# use predictive mean matching method for imputing
imputed_Data <- mice(airquality, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

completeData <- complete(imputed_Data)
completeData

# or we can do like this too
# in another way say do for variable Solar.R in airquality dataset

newair =airquality
dim(newair)
str(newair)
summary(newair)

# before imputing
hist(newair$Solar.R ,xlab = "Solar.R", ylab = "frequency",main="histogram of Solar.R",col="blue")
mean(newair$Solar.R)
mean(newair$Solar.R,na.rm = T)

# imputed my mean
newair$Solar.R[is.na(newair$Solar.R)]<- mean(newair$Solar.R,na.rm = T)
newair$Solar.R[is.na(newair$Solar.R)]
# check summary after done with imputing
summary(newair)
newair$Solar.R

# visualize after imputing the variable Solar.R with the mean
# lets visualize through histogram

# after imputing
hist(newair$Solar.R ,xlab = "Solar.R", ylab = "frequency",main="histogram of Solar.R",col="green")

# 10.1.5 bi-variate analysis
# How do Ozone and temperature measurements relate?
plot(airquality$Temp, airquality$Ozone, col="red")

pairs(airquality[,1:4])
plot(airquality$Temp, airquality$Ozone, col="red", pch =19) 

library(psych)
pairs.panels( airquality[,c(1,2,3,4,5,6,7)],
              method = "pearson", # correlation method
              hist.col = "blue",
              density = TRUE,  # show density plots
              ellipses = TRUE, # show correlation ellipses
              lm=TRUE,
              main ="Bivariate Analysys on airquality.csv using Pearson Correlation & Histogram Method"
)

# 10.1.6 Test relevant hypothesis for valid relations
# we will use paired test analysis on Ozone, Solar.R, Wind, Temp and apply null hypothesis theory
# Ho: Mean of first variable - Mean of 2 variable is equal to 0
# Ha: Mean of first variable - Mean of 2 variable is not equal to 0

t.test(x=airquality$Ozone, y=airquality$Solar.R ,alternative = "two.sided",mu=0 ,paired = TRUE)
t.test(x=airquality$Temp, y=airquality$Wind ,alternative = "two.sided",mu=0 ,paired = TRUE)
t.test(x=airquality$Ozone, y=airquality$Temp ,alternative = "two.sided",mu=0 ,paired = TRUE)
t.test(x=airquality$Day, y=airquality$Solar.R ,alternative = "two.sided",mu=0 ,paired = TRUE)

# since p value of this test is <0.05 we reject the null hypothesis and accept the alternative hypothesis Ha

# 10.1.7 Create cross tabulations with derived variables

# cross tabulate variables - Wind and Temperature
attach(airquality)
unique(Wind)
unique(Temp)

# derived variables of wind and temp

x<- cut(Wind,quantile(Wind))
x<- cut(Wind,breaks = seq(1,21,3),labels = c("wind1","wind2","wind3","wind4","wind5","wind6"))
y<- cut(Temp,quantile(Temp))
y<- cut(Temp,breaks = seq(55,100,9),labels = c("temp1","temp2","temp3","temp4","temp5"))
table(x,y)

# using xtabs function

mytable<- xtabs(~x+y,data = airquality)
mytable

# crosstabulate
install.packages("gmodels")
library(gmodels)
CrossTable(x,y)
