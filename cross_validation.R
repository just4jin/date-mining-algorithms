#######################################
#     cross-validation 
#######################################
install.packages("ISLR")
library(ISLR)
library(boot)

# training set
set.seed(1)
train=sample(392,196) # split the set of observations into halves

# linear regression
lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2) # MSE: 26.14142
# poly() estimate test error for polynomial and cubic regressions
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train) 
mean((mpg-predict(lm.fit2, Auto))[-train]^2) # MSE: 19.82259
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train) 
mean((mpg-predict(lm.fit3, Auto))[-train]^2) # MSE: 19.78252

# model with quadratic terms tend to perform better than with linear terms

#***************************************
# Leave-One-Out Cross-Validation (LOOCV)
#***************************************
glm.fit=glm(mpg~horsepower, data=Auto)
coef(glm.fit)
# (Intercept)  horsepower 
# 39.9358610  -0.1578447 
lm.fit=lm(mpg~horsepower, data=Auto)
coef(lm.fit)
# (Intercept)  horsepower 
# 39.9358610  -0.1578447 
cv.err=cv.glm(Auto,glm.fit)
names(cv.err)
# [1] "call"  "K"     "delta" "seed"
cv.err$delta # cv results
# [1] 24.23151 24.23114

cv.error=rep(0,5)
# iteratively fit polynomial regressions for polynomials of order i=1 to i=5
# computes the associated cross-validation error and stores in the ith element of the vector
# cv.error
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto, glm.fit)$delta[1]
}
cv.error# [1] 24.23151 19.24821 19.33498 19.42443 19.03321
plot(cv.error,type="l") # sharp drop in MSE from linear to quadratic, but not after

#**************************************
#   k-fold cross-validation
#**************************************
set.seed(17)
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10
# [1] 24.20520 19.18924 19.30662 19.33799 18.87911 19.02103 18.89609 19.71201 18.95140
# [10] 19.50196

plot(cv.error.10, type="l")

# similar to cv.error: sudden drop from linear to quadratic

