#############################################
#         Linear Discriminant Analysis (LDA)
#############################################
library(MASS)
library(ISLR)
attach(Smarket)
# training set
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005) # [1] 252   9
Direction.2005=Direction[!train]

lda.fit = lda(Direction~Lag1+Lag2, data=Smarket,subset=train)
lda.fit
# 
# Call:
#   lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
# 
# Prior probabilities of groups:
#   Down       Up 
# 0.491984 0.508016 
# 
# Group means:
#   Lag1        Lag2
# Down  0.04279022  0.03389409
# Up   -0.03954635 -0.03132544
# 
# Coefficients of linear discriminants:
#   LD1
# Lag1 -0.6420190
# Lag2 -0.5135293


# 49.2% of the training observations correspond to days during which market went down
# 50.8% ...went up.

plot(lda.fit)
# plots of linear discriminants obtained by computing -0.642*Lag1-0.514*Lag2
# for each of the training observations

lda.pred=predict(lda.fit,Smarket.2005)
names(lda.pred)
# [1] "class"     "posterior" "x"
# class:   LDA predictions about the movement of the market
# posterior:   matrix whose kth column contains posterior probability that correponding
# to observations belong to the kth class
# x:   linear discriminants

lda.class=lda.pred$class
table(lda.class, Direction.2005)
#         Direction.2005
# lda.class Down  Up
# Down       35  35
# Up         76 106

mean(lda.class==Direction.2005)
# [1] 0.5595238
# results same as logistic regression

sum(lda.pred$posterior[,1]>=.5) # 70
sum(lda.pred$posterior[,1]<.5) # 182
lda.pred$posterior[1:20,1] # probs that market will decrease
# 999      1000      1001      1002      1003      1004      1005      1006 
# 0.4901792 0.4792185 0.4668185 0.4740011 0.4927877 0.4938562 0.4951016 0.4872861 
# 1007      1008      1009      1010      1011      1012      1013      1014 
# 0.4907013 0.4844026 0.4906963 0.5119988 0.4895152 0.4706761 0.4744593 0.4799583 
# 1015      1016      1017      1018 
# 0.4935775 0.5030894 0.4978806 0.4886331 
lda.class[1:20]
# [1] Up   Up   Up   Up   Up   Up   Up   Up   Up   Up   Up   Down Up   Up   Up  
# [16] Up   Up   Down Up   Up  
# Levels: Down Up

# predict a market decrease only if we are very certain that market will
# indeed decrease on that that (probs is at least 90%)
sum(lda.pred$posterior[,1]>.9) # 0 none meet the creteria
