#####################################################
# Linear regression tutorial: Session 1
#
# File Created on: 21st March, 2018
# Created by: Adnane Ez-zizi
#             a.ez-zizi@bham.ac.uk
# 
# Most of the R outputs have been copied and pasted 
# as comments below their corresponding command lines. 
# The solutions to the three tasks that were given 
# during the session are also provided in this file.
#####################################################

#####################################################
# Setting the working directory 
#####################################################

setwd("~/Teaching/Linear_regression_tutorial/Part1_SimpleLinearRegression")

#####################################################
# Advertising dataset (code used during the lecture)
#####################################################

### Load the advertising dataset 
Advertising = read.csv("Advertising.csv")
head(Advertising, n=3)
#   X    TV radio newspaper sales
# 1 1 230.1  37.8      69.2  22.1
# 2 2  44.5  39.3      45.1  10.4
# 3 3  17.2  45.9      69.3   9.3

### Scatter plot of sales as a function of TV budget
plot(Advertising$TV, Advertising$sales,
     main = "Relationship between TV advertising and the sales",
     xlab = "TV advertising",
     ylab = "Sales")

### Fit a simple linear regression model
lm_adv = lm(sales ~ TV, data = Advertising)
summary(lm_adv)
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

### Adding the regression line to the scatter plot
plot(Advertising$TV, Advertising$sales,
     main = "Relationship between TV advertising and the sales",
     xlab = "TV advertising",
     ylab = "Sales")
abline(lm_adv$coefficients, col = "red")

### Confidence intervals for the slope
?confint
confint(lm_adv, "TV")
#         2.5 %     97.5 %
# TV 0.04223072 0.05284256

#####################################################
# Tasks using the mlu dataset
#####################################################

############## Task 1 ##############

### Load the mlu dataset
mlu = read.table("mlu.txt")
head(mlu, n=3)
#   age  chi  mot
# 1  25 1.46 5.42
# 2  26 1.41 5.69
# 3  27 1.66 6.27

### Number of variables
ncol(mlu) # 3
### Number of observations
nrow(mlu) # 24

############## Task 2 ##############

### Scatter plot of the mother's MLU as a function of the child's MLU
plot(mlu$chi, mlu$mot, 
     main = "Relationship between the mother's MLU and the child's MLU",
     xlab = "Child's MLU",
     ylab = "Mother's MLU")

############## Task 3 ##############

### Fit a simple linear regression model
lm_mlu = lm(mot ~ chi, data = mlu)
summary(lm_mlu)
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

### Adding the regression line to the scatter plot
plot(mlu$chi, mlu$mot, 
     main = "Relationship between the mother's MLU and the child's MLU",
     xlab = "Child's MLU",
     ylab = "Mother's MLU")
abline(lm_mlu$coefficients, col = "red")

### Confidence intervals for the slope
confint(lm_mlu, "chi")
#           2.5 %    97.5 %
# chi -0.02374067 0.3242654

