##############################################
#         K-Nearest Neighbours (KNN)
##############################################
library(ISLR)
library(class)
library(MASS)

dim(Caravan)
attach(Caravan)
summary(Purchase)
# No  Yes 
# 5474  348 

# only 6% of people purchased caravan insurance

# KNN classifier predict the class of a given test observation by identifiying the 
# observations that are nearest to it, the scale of variables matters

# large-scale variables have larger effect on distance b/t observations 

# standardize the data to set mean to be zero and std as 1 for all variables
standardized.X=scale(Caravan[,-86])
var(Caravan[,1]) # [1] 165.0378
var(Caravan[,2]) # [1] 0.1647078
var(standardized.X[,1]) # 1
var(standardized.X[,2]) # 1

test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)

# K = 1
knn.pred=knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred) # 0.118
mean(test.Y!="No") # 0.059

# confusion matrix
table(knn.pred, test.Y)
#         test.Y
# knn.pred  No Yes
# No      873  50
# Yes     68   9
9/(68+9) # [1] 0.1168831 ~ 11.7% purchase rate

# K = 3
knn.pred=knn(train.X, test.X, train.Y, k=3)
mean(test.Y!=knn.pred) # 0.075
mean(test.Y!="No") # 0.059
table(knn.pred, test.Y)
# test.Y
# knn.pred  No Yes
# No  920  54
# Yes  21   5
5/26 # 0.1923077 ~ 19% purchase rate

# K = 5
knn.pred=knn(train.X, test.X, train.Y, k=5)
mean(test.Y!=knn.pred) # 0.066
mean(test.Y!="No") # 0.059
table(knn.pred, test.Y)
# test.Y
# knn.pred  No Yes
# No  930  55
# Yes  11   4

4/15 # 0.2666667 ~ 26.7% purchase rate



