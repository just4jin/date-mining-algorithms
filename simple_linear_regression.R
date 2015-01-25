########################################
#     Simple Linear Regression
########################################

install.packages("ISLR")
library(MASS)
library(ISLR)

# records median house value for 506 neighbourhoods around Boston
fix(Boston)
names(Boston)
# [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"    
# [8] "dis"     "rad"     "tax"     "ptratio" "black"   "lstat"   "medv"   

lm.fit <- lm(medv ~ lstat, data = Boston)
attach(Boston)
lm.fit <- lm(medv ~ lstat)
lm.fit
# Call:
#   lm(formula = medv ~ lstat)
# 
# Coefficients:
#   (Intercept)        lstat  
# 34.55        -0.95  

summary(lm.fit)
# Call:
#   lm(formula = medv ~ lstat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -15.168  -3.990  -1.318   2.034  24.500 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    34.55384    0.56263   61.41   <2e-16 ***
#   lstat       -0.95005    0.03873  -24.53   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 6.216 on 504 degrees of freedom
# Multiple R-squared:  0.5441,  Adjusted R-squared:  0.5432 
# F-statistic: 601.6 on 1 and 504 DF,  p-value: < 2.2e-16

names(lm.fit)
# [1] "coefficients"  "residuals"     "effects"       "rank"         
# [5] "fitted.values" "assign"        "qr"            "df.residual"  
# [9] "xlevels"       "call"          "terms"         "model" 

confint(lm.fit)
#                2.5 %     97.5 %
# (Intercept) 33.448457 35.6592247
# lstat       -1.026148 -0.8739505

# predict intervals for the prediction of medv for a given value of lstat
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = "confidence")
#      fit      lwr      upr
# 1 29.80359 29.00741 30.59978
# 2 25.05335 24.47413 25.63256
# 3 20.30310 19.73159 20.87461

predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = "prediction")
#     fit       lwr      upr
# 1 29.80359 17.565675 42.04151
# 2 25.05335 12.827626 37.27907
# 3 20.30310  8.077742 32.52846

# 95% confidence intervale associated with a lstat value of 10 is (24.47, 25.63)
# 95% prediction interval is (12.828, 37.28)

# scatterplot + least squares regression line
plot(Boston$lstat, Boston$medv)
abline(lm.fit, lwd=2, col=2) # add least squares regression line
abline(lstat, medv, pch=20)

# diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)

# residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit)) # some evidence of non=linearity

# leverage stats
plot(hatvalues(lm.fit))
# identify the index of the largest leverage statistics of a vector
which.max(hatvalues(lm.fit)) # 375 
