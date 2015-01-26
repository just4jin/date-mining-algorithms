####################################
#     Bootstrap
####################################

library(ISLR)
library(boot)

# compute alpha based on input (X,Y) values
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

# estimate alpha using 100 observations
alpha.fn(Portfolio, 1:100) # [1] 0.5758321

# randomly select 100 observations from the range 1 to 100 with replacement
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T)) # [1]  0.5963833

# boot() automate the above process for bootstrap
boot(Portfolio, alpha.fn, R=1000)
# ORDINARY NONPARAMETRIC BOOTSTRAP
# 
# 
# Call:
#   boot(data = Portfolio, statistic = alpha.fn, R = 1000)
# 
# 
# Bootstrap Statistics :
#       original        bias    std. error
# t1*  0.5758321 -7.315422e-05  0.08861826

# using the original data alpha=0.5758
# bootstrap estimate for SE(alpha)=0.0886

# access the accuracy of a linear regression model
boot.fn=function(data, index){
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
}
# create the bootstrap estimates for the intercept and slope terms by 
# randomly sampling from among the observations with replacement
boot.fn(Auto, 1:392) 
# (Intercept)  horsepower 
# 39.9358610  -0.1578447 

# compute std error or 1000 bootstrap estimates for intercept and slope
boot(Auto, boot.fn, 1000)
# ORDINARY NONPARAMETRIC BOOTSTRAP
# 
# 
# Call:
#   boot(data = Auto, statistic = boot.fn, R = 1000)
# 
# 
# Bootstrap Statistics :
#         original        bias    std. error
# t1*   39.9358610  0.0126152644 0.871267432
# t2*   -0.1578447 -0.0002691801 0.007540188

# SE(alpha)=0.871267432, SE(beta1)=0.007540188

summary(lm(mpg~horsepower, data=Auto))$coef
#               Estimate  Std. Error   t value      Pr(>|t|)
# (Intercept) 39.9358610 0.717498656  55.65984 1.220362e-187
# horsepower  -0.1578447 0.006445501 -24.48914  7.031989e-81

# intercept=0.717498656 , slope=0.006445501

# trying out quadratic model fit
boot.fn=function(data, index){
  return(coef(lm(mpg~horsepower+I(horsepower^2), data=data, subset=index)))
}
set.seed(1)
boot(Auto, boot.fn, 1000)
# ORDINARY NONPARAMETRIC BOOTSTRAP
# 
# 
# Call:
#   boot(data = Auto, statistic = boot.fn, R = 1000)
# 
# 
# Bootstrap Statistics :
#   original        bias     std. error
# t1* 56.900099702  6.098115e-03 2.0944855842
# t2* -0.466189630 -1.777108e-04 0.0334123802
# t3*  0.001230536  1.324315e-06 0.0001208339
summary(lm(mpg~horsepower+I(horsepower^2)))
# Call:
#   lm(formula = mpg ~ horsepower + I(horsepower^2))
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -14.7135  -2.5943  -0.0859   2.2868  15.8961 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     56.9000997  1.8004268   31.60   <2e-16 ***
#   horsepower      -0.4661896  0.0311246  -14.98   <2e-16 ***
#   I(horsepower^2)  0.0012305  0.0001221   10.08   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.374 on 389 degrees of freedom
# Multiple R-squared:  0.6876,  Adjusted R-squared:  0.686 
# F-statistic:   428 on 2 and 389 DF,  p-value: < 2.2e-16

# better correspondence between bootstrap estimates and standard estimates for SE