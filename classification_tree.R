#####################################
#       Classification Trees
#####################################
install.packages("tree")
library(ISLR)
library(MASS)
library(tree)

attach(Carseats)
# variable High="No" when Sales<=8, otherwise "Yes"
High=ifelse(Sales<=8, "No","Yes")

# merge together
Carseats=data.frame(Carseats, High)

# predict High using all variables but Sales by fitting
# a classificaiton tree
tree.carseats=tree(High~.-Sales, Carseats)
summary(tree.carseats)
# Classification tree:
#
#   tree(formula = High ~ . - Sales, data = Carseats)
# Variables actually used in tree construction:
# [1] "ShelveLoc"   "Price"       "Income"      "CompPrice"  
# [5] "Population"  "Advertising" "Age"         "US"  
#
# Number of terminal nodes:  27 
# Residual mean deviance:  0.4575 = 170.7 / 373 
# Misclassification error rate: 0.09 = 36 / 400 

# plot the tree
plot(tree.carseats)
text(tree.carseats,pretty=0)

# the most important indicator of Sales appears to be shelving location
# it differentiates Good locations from Bad and Medium

tree.carseats

# estimate the performance of a classification tree
# we need to estimate the test error
set.seed(2)
# train set
train=sample(1:nrow(Carseats),200)
# test set
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales, Carseats, subset=train)
tree.pred=predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
#            High.test
# tree.pred   No Yes
#        No  89  29
#        Yes 27  55

(86+57)/(200)
# 0.715

# prune the tree using cv
set.seed(3)
cv.carseats=cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
# [1] "size"   "dev"    "k"      "method"
cv.carseats
# $size
# [1] 19 17 14 13  9  7  3  2  1
# 
# $dev
# [1] 55 55 53 52 50 56 69 65 80
# 
# $k
# [1]       -Inf  0.0000000  0.6666667  1.0000000  1.7500000
# [6]  2.0000000  4.2500000  5.0000000 23.0000000
# 
# $method
# [1] "misclass"
# 
# attr(,"class")
# [1] "prune"         "tree.sequence"

# plot error rate as function of size and k
# lowest cv error rate obtained by size=50, k=9
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

# prune the tree to obtain the nine-node tree
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

# access performance of pruned tree on test data
tree.pred=predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
# High.test
# tree.pred No Yes
# No  94  24
# Yes 22  60

(94+60)/200 # 77% correctly classified

# increase the # of nodes to be 15
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats, pretty=0)

# access performance of pruned tree on test data
tree.pred=predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
# High.test
# tree.pred No Yes
# No  86  22
# Yes 30  62
(86+62)/200 # 74% correctly classified

# increase the number of nodes improved results


