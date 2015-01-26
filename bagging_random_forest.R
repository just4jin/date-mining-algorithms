##########################################
#         Random Forest
##########################################
install.packages("randomForest")
library(randomForest)
library(MASS)

dim(Boston) # 506 14
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
boston.test=Boston[-train,"medv"]

# bagging : like random forest but m=p
set.seed(1)
# mtry: number of predictors used to split the tree
bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=13, importance=T)
bag.boston
# Call:
#   randomForest(formula = medv ~ ., data = Boston, mtry = 13, importance = T,      subset = train) 
#     Type of random forest: regression
#     Number of trees: 500
#     No. of variables tried at each split: 13
# 
#      Mean of squared residuals: 11.02509
#      % Var explained: 86.65

# how well model performs on test set?
yhat.bag=predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1) # intercept slope
mean((yhat.bag-boston.test)^2) # [1] 13.47349

# change the number of trees grown by randomForest()
bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25)
yhat.bag=predict(bag.boston, newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)# [1] 13.85203

# random forest
set.seed(1)
rf.boston=randomForest(medv~., data=Boston, subset=train, mtry=6, importance=T)
yhat.rf=predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2) # MSE: 11.25961
# yield an improvement over bagging in the case
importance(rf.boston) # importance of each variable
#           %IncMSE IncNodePurity
# crim    13.086497    1156.49765
# zn       2.750867      74.36038
# indus    9.977787     994.97440
# chas     3.044812      47.67339
# nox     12.863522    1087.24104
# rm      29.866295    6075.35428
# age      9.722589     544.35682
# dis     14.828196    1328.61056
# rad      3.897841     109.46691
# tax      8.915545     436.71255
# ptratio 11.610930    1091.45251
# black    5.920896     321.32081
# lstat   29.729360    7370.83288

varImpPlot(rf.boston)
# lstat and rm are two important variables
