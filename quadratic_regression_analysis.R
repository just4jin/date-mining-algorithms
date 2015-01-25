##############################################
#         Quadratic Discriminant Analysis
##############################################
library(MASS)
library(ISLR)

attach(Smarket)

# training set
train=(Year<2005)
# test set
Smarket.2005=Smarket[!train,]

# qda model
qda.fit=qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
# Call:
#   qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
# 
# Prior probabilities of groups:
#   Down       Up 
# 0.491984 0.508016 
# 
# Group means:
#   Lag1        Lag2
# Down  0.04279022  0.03389409
# Up   -0.03954635 -0.03132544

# no coefficient because QDA classifier involves a quadratic rather than linear function of predictors

qda.class=predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
# Direction.2005
# qda.class Down  Up
# Down   30  20
# Up     81 121
mean(qda.class==Direction.2005)
# [1] 0.5992063

# quite an improvement from lda
# suggests the true relationship my be more quadratic rather than linear