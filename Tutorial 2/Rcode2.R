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

### Load the mlu dataset using read.table() (to complete)




### Description of the mlu dataset:
# During language acquisition, it is claimed that caregivers adapt 
# their language to the language abilities of children. The data set 
# follows a particular child and record an hour conversation between 
# the child and the mother once every month between the child's second 
# and fourth birth dates. 
# The variables chi and mot record, for both the child and the mother, 
# a measure of language complexity/competency for children called Mean 
# Length of Utterance (MLU).

###########################################
# Simple linear regression models
###########################################

### Regress sales on TV using the advertising dataset
slr_adv = lm(sales ~ TV, data = Advertising)
summary(slr_adv)

### Regress child's mlu on mother's mlu using the mlu 
### dataset (to complete)



###########################################
# Confidence intervals
###########################################

?confint

### Confidence interval for the TV coefficient using the advertising dataset
confint(slr_adv, "TV")

### Confidence interval for the chi coefficient using the 
### mlu dataset (to complete)


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


# Fit a multiple linear regression model (to complete)


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








