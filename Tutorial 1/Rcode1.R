###################################################
# Linear regression tutorial
#
# File Created on: 21st March, 2018
# Created by: Adnane Ez-zizi
#             a.ez-zizi@sheffield.ac.uk
# 
###################################################

###########################################
# Setting the working directory and loading
# necessary packages
###########################################

setwd("C:/Users/Adnane/Documents/PostDoc Sheffield/Teaching/Linear regression");

###########################################
# Advertising dataset
###########################################

### Load the advertising dataset 
Advertising = read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")
head(Advertising, n=3)

### Scatter plot of sales as a function of TV budget
plot(Advertising$TV, Advertising$sales)

### Fit a simple linear regression model
lm_adv = lm(sales ~ TV, data = Advertising)
summary(lm_adv)

### Adding the regression line to the scatter plot
plot(Advertising$TV, Advertising$sales)
abline(lm_adv$coefficients, col = "red")

###########################################
# mlu dataset
###########################################

### Load the mlu dataset
mlu = read.table("mlu.txt")
head(mlu, n=3)

### Scatter plot of the mother's MLU as a function of the child's MLU
plot(mlu$chi, mlu$mot)

### Fit a simple linear regression model
lm_mlu = lm(mot ~ chi, data = mlu)
summary(lm_mlu)

### Adding the regression line to the scatter plot
plot(mlu$chi, mlu$mot)
abline(lm_mlu$coefficients, col = "red")



