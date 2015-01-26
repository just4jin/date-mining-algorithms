##################################
#       Regression Tree
##################################
library(MASS)
library(ISLR)

set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston=tree(medv~., Boston, subset=train)
summary(tree.boston)
# Regression tree:
#   tree(formula = medv ~ ., data = Boston, subset = train)
# Variables actually used in tree construction:
#   [1] "lstat" "rm"    "dis"  
# Number of terminal nodes:  8 
# Residual mean deviance:  12.65 = 3099 / 245 
# Distribution of residuals:
#   Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -14.10000  -2.04200  -0.05357   0.00000   1.96000  12.60000 

# only three variables used to construct the tree
# deviance: sum of squared errors 

plot(tree.boston)
text(tree.boston, pretty=0)

# prune the tree
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')

# prune to have 5 nodes
prune.boston=prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

# use unpruned tree to make prediction on test set
yhat=predict(tree.boston, newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2) #MSE: 25.04559
