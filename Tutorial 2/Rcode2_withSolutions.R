###################################################
# Linear regression: tutorial 2
#
# File Created on: 16th May, 2018
# Created by: Adnane Ez-zizi
#             a.ez-zizi@sheffield.ac.uk
# 
###################################################

###########################################
# Setting the working directory and loading
# necessary packages
###########################################

# DON'T FORGET TO MODIFY THE WORKING DIRECTORY
setwd("C:/Users/Adnane/Documents/PostDoc Sheffield/Teaching/Linear regression/Tutorial 2");

###########################################
# Load the datasets
###########################################

### Load the advertising dataset 
Advertising = read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")
head(Advertising, n=3)

### Load the mlu dataset
mlu = read.table("mlu.txt")
head(mlu, n=3)

###########################################
# Simple linear regression models
###########################################

### advertising dataset
slr_adv = lm(sales ~ TV, data = Advertising)
summary(slr_adv)

### mlu dataset
slr_mlu = lm(mot ~ chi, data = mlu)
summary(slr_mlu)

###########################################
# Confidence intervals
###########################################

?confint

### advertising dataset
confint(slr_adv, "TV")

### mlu dataset (to complete)
confint(slr_mlu, "chi")

###########################################
# Multiple linear regression
###########################################

### advertising dataset

# Plot all predictors against the response 
par(mfrow=c(1,3))
plot(sales~TV, Advertising)
plot(sales~radio, Advertising)
plot(sales~newspaper, Advertising)
par(mfrow=c(1,1))

# Fit a multiple linear regression model
mlr_adv = lm(sales ~ TV + radio + newspaper, data = Advertising)
summary(mlr_adv)

### mlu dataset (to complete)

# Plot all predictors against the response (to complete)
par(mfrow=c(1,2))
plot(mot~chi, mlu)
plot(mot~age, mlu)
par(mfrow=c(1,1))

# Fit a multiple linear regression model (to complete)
mlr_mlu = lm(mot ~ chi + age, data = mlu)
summary(mlr_mlu)

###########################################
# Predictions
###########################################

?predict.lm

### advertising dataset 

# Is it better to invest £100,000 on TV alone or £50000 on both TV and radio?
predict.lm(slr_adv, data.frame(TV=c(100,50), radio=c(0,50)))

# Prediction given as confidence intervals
predict.lm(slr_adv, data.frame(TV=c(100,50), radio=c(0,50)), interval = "confidence")

### mlu dataset (to complete)

# Predict the mother's mlu at age 50 months if the child's mlu is 3.7
predict.lm(slr_mlu, data.frame(age=c(50), chi=c(3.8)))

###########################################
# Interactions
###########################################

### advertising dataset

# Add an interaction term between TV and radio to the multiple 
# linear regression
mlr_adv_inter = lm(sales ~ TV * radio + newspaper, data = Advertising)
summary(mlr_adv_inter)

### mlu dataset (to complete)
# Add an interaction term between the child's mlu and age
mlr_mlu_inter = lm(mot ~ chi * age, data = mlu)
summary(mlr_mlu_inter)




