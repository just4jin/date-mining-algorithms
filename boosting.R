######################################
#           Boosting
######################################
install.packages("gbm")
library(gbm)
library(MASS)
library(ISLR)

set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
boston.test=Boston[-train,"medv"]
# boosting: distribution=gaussian for regression probs, bernoulli for classification probs
boost.boston=gbm(medv~., data=Boston[train,], distribution="gaussian",n.trees=5000, interaction.depth=4)
summary(boost.boston)
# var             rel.inf
# lstat     lstat 46.5750123
# rm           rm 30.7136292
# dis         dis  6.7046057
# crim       crim  4.0022999
# nox         nox  2.6463615
# ptratio ptratio  2.3558616
# black     black  1.7742596
# age         age  1.6161692
# tax         tax  1.3448183
# indus     indus  1.2734992
# chas       chas  0.7623708
# rad         rad  0.2119990
# zn           zn  0.0191136

# plot relative influence: lstat, rm

par(mfrow=c(1,2))
# partial dependence plots
# marginal effect of the variable on response
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")
# relationship: increasing with rm, decreasing with lstat

# predict with test data set
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2) # MSE: 11.84694

# boosting with a different shrinkage parameter (default=0.01), change to 0.2
boost.boston=gbm(medv~., data=Boston[train,], distribution="gaussian",
                 n.trees=5000, interaction.depth=4, shrinkage=0.2, verbose=F)
# MSE: 11.42312 slight improvement

