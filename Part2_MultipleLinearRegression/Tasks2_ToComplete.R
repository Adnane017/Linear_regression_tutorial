###############################################################
# Linear regression tutorial: Session 2
#
# File Created on: 16th May, 2018
# Created by: Adnane Ez-zizi
#             a.ez-zizi@bham.ac.uk
# 
# The R outputs have been copied and pasted as 
# comments below their corresponding command lines. 
# You will need to fill in the blanks reserved for 
# each of the three tasks. You can use the code that 
# is provided for the advertising data set. 
###############################################################

###############################################################
# Setting the working directory 
###############################################################

# DON'T FORGET TO MODIFY THE WORKING DIRECTORY
setwd("~/Teaching/Linear_regression_tutorial/Part2_MultipleLinearRegression");

###############################################################
# Load the datasets
###############################################################

### Load the advertising dataset 
Advertising = read.csv("Advertising.csv")
head(Advertising, n=3)
#   X    TV radio newspaper sales
# 1 1 230.1  37.8      69.2  22.1
# 2 2  44.5  39.3      45.1  10.4
# 3 3  17.2  45.9      69.3   9.3

### Load the mlu dataset
mlu = read.table("mlu.txt")
head(mlu, n=3)
#   age  chi  mot
# 1  25 1.46 5.42
# 2  26 1.41 5.69
# 3  27 1.66 6.27

###############################################################
# Simple linear regression models (Recap from our last session)
###############################################################

### advertising dataset
slr_adv = lm(sales ~ TV, data = Advertising)
summary(slr_adv)
# Call:
# lm(formula = sales ~ TV, data = Advertising)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -8.3860 -1.9545 -0.1913  2.0671  7.2124 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 7.032594   0.457843   15.36   <2e-16 ***
# TV          0.047537   0.002691   17.67   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.259 on 198 degrees of freedom
# Multiple R-squared:  0.6119,	Adjusted R-squared:  0.6099 
# F-statistic: 312.1 on 1 and 198 DF,  p-value: < 2.2e-16

# Confidence interval
confint(slr_adv, "TV")
#         2.5 %     97.5 %
# TV 0.04223072 0.05284256

### mlu dataset
slr_mlu = lm(mot ~ chi, data = mlu)
summary(slr_mlu)
# Call:
# lm(formula = mot ~ chi, data = mlu)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.79928 -0.14665  0.06142  0.14003  0.66232 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   5.7133     0.2326  24.559   <2e-16 ***
# chi           0.1503     0.0839   1.791   0.0871 .  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3182 on 22 degrees of freedom
# Multiple R-squared:  0.1272,	Adjusted R-squared:  0.08757 
# F-statistic: 3.207 on 1 and 22 DF,  p-value: 0.08708

# Confidence interval
confint(slr_mlu, "chi")
#           2.5 %    97.5 %
# chi -0.02374067 0.3242654

###############################################################
# Multiple linear regression
###############################################################

### advertising dataset

# Plot all predictors against the response variable
par(mfrow=c(1,3))
plot(sales~TV, Advertising)
plot(sales~radio, Advertising)
plot(sales~newspaper, Advertising)
par(mfrow=c(1,1))

# Fit a multiple linear regression model
mlr_adv = lm(sales ~ TV + radio + newspaper, data = Advertising)
summary(mlr_adv)
# Call:
# lm(formula = sales ~ TV + radio + newspaper, data = Advertising)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -8.8277 -0.8908  0.2418  1.1893  2.8292 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.938889   0.311908   9.422   <2e-16 ***
# TV           0.045765   0.001395  32.809   <2e-16 ***
# radio        0.188530   0.008611  21.893   <2e-16 ***
# newspaper   -0.001037   0.005871  -0.177     0.86    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.686 on 196 degrees of freedom
# Multiple R-squared:  0.8972,	Adjusted R-squared:  0.8956 
# F-statistic: 570.3 on 3 and 196 DF,  p-value: < 2.2e-16

# Predictions based on the model
# Is it better to invest £100,000 on TV alone or £50000 on both TV and radio?
predict.lm(mlr_adv, data.frame(TV=c(100,50), radio=c(0,50), newspaper=c(0,0)), 
           interval = "confidence")
#         fit       lwr       upr
# 1  7.515354  7.018004  8.012704
# 2 14.653622 13.897036 15.410209
# We have added the option interval = "confidence" to get confidence intervals for 
# the predictions. The results show that it is better to divide the budget into
# both TV and radio advertising

############## Task 1: mlu dataset (to complete) ##############

# Plot all predictors against the response variable (to complete)


# Fit a multiple linear regression model (to complete)


###############################################################
# Adding an interaction term
###############################################################

### advertising dataset

# Add an interaction term between TV and radio to the multiple 
# linear regression
mlr_adv_inter = lm(sales ~ TV * radio + newspaper, data = Advertising)
summary(mlr_adv_inter)
# Call:
# lm(formula = sales ~ TV * radio + newspaper, data = Advertising)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -6.2929 -0.3983  0.1811  0.5957  1.5009 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 6.728e+00  2.533e-01  26.561  < 2e-16 ***
# TV          1.907e-02  1.509e-03  12.633  < 2e-16 ***
# radio       2.799e-02  9.141e-03   3.062  0.00251 ** 
# newspaper   1.444e-03  3.295e-03   0.438  0.66169    
# TV:radio    1.087e-03  5.256e-05  20.686  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9455 on 195 degrees of freedom
# Multiple R-squared:  0.9678,	Adjusted R-squared:  0.9672 
# F-statistic:  1466 on 4 and 195 DF,  p-value: < 2.2e-16

############## Task 2: mlu dataset (to complete) ##############

# Add an interaction term between the child's mlu and age





