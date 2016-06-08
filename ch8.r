# Lab: Decision Trees
library(tree)
library(ISLR)

attach(Carseats)

High <- ifelse(Sales <= 8, "No", "Yes")

Carseats <- data.frame(Carseats, High)

tree.carseats <- tree(High~.-Sales,data=Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty=0)

tree.carseats

set.seed(2)

train <- sample(1:nrow(Carseats),200)

Carseats.test <- Carseats[-train,]
High.test <- High[-train]

tree.carseats <- tree(High~.-Sales, data=Carseats, subset=train)
tree.pred <- predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)

set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)

par(mfrow=c(2,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

prune.carseats <- prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

prune.carseats <- prune.misclass(tree.carseats, best=15)
plot(prune.carseats)
text(prune.carseats, pretty=0)
tree.pred=predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(86+62)/200

# Lab: Regression trees

library(MASS)
set.seed(1)

train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty=0)

cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')

prune.boston <- prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

yhat <- predict(tree.boston, newdata=Boston[-train,])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

# Lab: Bagging and Random Forests
library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv~.,data=Boston, subset=train, mtry=13, importance=TRUE)
bag.boston

yhat.bag <- predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

set.seed(1)
rf.boston <- randomForest(medv~.,data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf <- predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
importance(rf.boston)
varImpPlot(rf.boston)

# Lab: Boosting
library(gbm)
set.seed(1)
boost.boston <- gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4)
summary(boost.boston)

par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")
yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)
boost.boston <- gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4, shrinkage=0.2,verbose=F)
yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)    
mean((yhat.boost-boston.test)^2)

# Conceptual Exercises
# 2.
# We are fitting a model to the residuals, not the final value of y.
# We want the residuals to be as small as possible, and each iteration of the
# fitting process creates a model that gets as close as possible to predicting the 
# entire remaining residual, and then the predicted residual is subtracted away from
# the actual residual until there is very little remaining residual. The residual
# starts as the 'true' value of y since the model is the null model. Each 
# "mini-model" predicts a bit of the value of y, until you have a series of
# models that together predict the full value of y for a given example. 
# 3.

pm1 <- seq(.1,1,.1)
pm2 <- 1 - pm1
df <- data.frame(pm1, pm2)

rowmax <- apply(df, 1, max)
df$classification.error <- 1 - rowmax
df$gini <- df$pm1*(1-df$pm1) + df$pm2*(1-df$pm2)
df$cross.entropy <- -(df$pm1*log(df$pm1)+df$pm2*log(df$pm2))

plot(df$pm1, df$cross.entropy, type="l")
lines(df$pm1, df$classification.error, type="l")
lines(df$pm1, df$gini)

# 4 requires pencil and paper.

# 5.
results <- c(0.1,0.15,0.2,0.2,0.55,0.6,0.6,0.65,0.7,0.75)
is_red_majority <- sum(results > .5)
is_red_mean <- mean(results)
# In this case, for majority vote we classify as red (6/10) but for
# avg probability we classify as green (0.45).

# 6.
# First we scan all of the variables along with several pre-defined cut points
# for each variable to figure out which split in the data would improve our
# measure of 'randomness' the most (i.e. the cross-entropy or some other measure).
# Then we continue doing so recursively until we have grown a large tree to a
# pre-specified number of terminal nodes. Then we can use something like
# weakest link pruning whereby for each terminal node we see if removing it
# altogether would improve the MSE penalized by the number of terminal nodes.
# We do this until we minimize this size-penalized MSE calculation.

# 7. 
all_mtrys <- seq(1,10)
all_ntrees <- seq(5,100,5)

Boston$test <- rep(FALSE, nrow(Boston))
Boston$test[sample(nrow(Boston), nrow(Boston)/3,FALSE)] <- TRUE

test.errors <- c()
for(i in all_mtrys) {
    rfm <- randomForest(medv~., data=Boston[Boston$test==FALSE,], mtry=i)
    predictions <- predict(rfm, Boston[Boston$test==TRUE,])
    test.error <- mean((predictions - Boston[Boston$test==TRUE,"medv"])^2)
    test.errors <- c(test.errors, test.error)
}

par(mfrow=c(1,2))
plot(all_mtrys, test.errors, type="l", col="blue")

test.errors <- c()
for(i in all_ntrees) {
    rfm <- randomForest(medv~., data=Boston[Boston$test==FALSE,], ntree=i)
    predictions <- predict(rfm, Boston[Boston$test==TRUE,])
    test.error <- mean((predictions - Boston[Boston$test==TRUE,"medv"])^2)
    test.errors <- c(test.errors, test.error)
}

plot(all_ntrees, test.errors, type="l", col="blue")

# Seemingly mtrys causes the test error rate to decrease in a relatively
# smooth way while ntrees causes the test error rate to decrease in a jagged
# way. 

# 8.
# a.
train <- rep(FALSE, nrow(Carseats))
train[sample(nrow(Carseats), nrow(Carseats)/2, FALSE)] <- TRUE

train.set <- Carseats[train,]
test.set <- Carseats[!train,]
# b. 
tr <- tree(Sales~.,data=train.set)
summary(tr)
plot(tr)
text(tr, pretty=0)

# it appears the High is the most important variable. ShelveLoc seems
# to have an indirect relationship with the response variable, and so
# does price. Higher populations seem to correlate with higher sales.

test.mse <- mean((test.set$Sales - predict(tr, newdata=test.set))^2)
test.mse

cv.cstree <- cv.tree(tr)
plot(cv.cstree$size, cv.cstree$dev,type='b')

newtr <- prune.tree(tr, best=5)
new.test.mse <- mean((test.set$Sales - predict(newtr, newdata=test.set))^2)
new.test.mse

# yes, this slightly improves test mse. 

# d.
bag.model <- randomForest(Sales~., data=train.set, mtry=ncol(train.set)-1)
test.mse <- mean((test.set$Sales - predict(bag.model, newdata=test.set))^2)
test.mse

# With a test MSE of 2.04, we beat out a regular decision tree.

# e.
rf.model <- randomForest(Sales~., data=train.set)
test.mse <- mean((test.set$Sales - predict(rf.model, newdata=test.set))^2)
test.mse
# this is by far the best of all, with a test mse of 1.76.

test.mses <- c()
for(i in seq(1:(ncol(train.set)-1))) {
    rf.model <- randomForest(Sales~., data=train.set, mtry=i)
    test.mses <- c(test.mses, mean((test.set$Sales - predict(rf.model, newdata=test.set))^2))
}

plot(seq(ncol(train.set)-1,1,-1), test.mses, xlab="mtry", ylab="Test MSE", type="l")

# MSE seems to go down initially and reach a minimum around 9 vars
# and then slowly start to increase until maxing out around 3.

# 9.
# a.
train.vec <- sample(nrow(OJ), 800)
train <- rep(FALSE,nrow(OJ))
train[train.vec] <- TRUE

# b.
tree.model <- tree(Purchase~.,data=OJ, subset=train)

summary(tree.model)
# The training error rate is 16.9%
# There are 9 terminal nodes.

# c.
tree.model
# Node 10 shows where SalePriceMM < 2 million, there are 157
# examples, the deviance is 166, the predicted class is MM, and the class
# probabilities are .22 and .77 respectively. 

# d.
plot(tree.model)
text(tree.model, pretty=0)
# Seems like LoyalCH is the most important variable by far,
# being the only one used in the first two layers of the tree. 
# It seems like a lower LoyalCH is correlated with MM, whereas
# a higher LoyalCH is correlated with CH.

# e. 
predictions <- predict(tree.model, newdata=OJ[!train,])[,"CH"]
actuals <- OJ[!train,]$Purchase == "CH"
mse <- mean((predictions - actuals)^2)
# The test error rate is 0.1201726
=======
predictions <- predict(tree.model, newdata=OJ[!train,], type="class")
actuals <- OJ[!train,]$Purchase
misclass.unpruned <- sum(predictions != actuals)
misclass.unpruned / length(predictions)
summary(tree.model)
# train error rate is .1688
# test error rate is 0.1481481

# f.
cv.tree.results <- cv.tree(tree.model, FUN=prune.misclass)
cv.tree.results

#. g
plot(cv.tree.results)

# h. 
# Looks like the optimal tree size is 7 terminal nodes
# 7 terminal nodes is the simplest we can get and still
# maintain our best misclassification error

new.cv.tree <- prune.tree(tree.model, best=7)

new.predictions <- predict(new.cv.tree, newdata=OJ[!train,], type="class")
pruned.misclass <- sum(new.predictions != actuals)
pruned.misclass/length(new.predictions)
summary(new.cv.tree)
# train error rate is .19
# test error rate is .177
# these are worse than the unpruned model
# some experimentation shows that 8 nodes is equivalent
# to the unpruned model.
# either way, pruning isn't helping much.

# 10.
# a.
hitters.clean <- Hitters[!is.na(Hitters$Salary),]

# b.
nrow(hitters.clean)
nrow(Hitters)
train <- rep(FALSE, nrow(hitters.clean))
train[1:200] <- TRUE

library(gbm)
set.seed(1)

# c.

train.mses <- c()
for(i in seq(0.1,1,0.1)) {
    boost.hitters <- gbm(log(Salary)~., 
                         data=hitters.clean[train,], 
                         distribution="gaussian",
                         n.trees=1000,
                         shrinkage=i)
    predictions <- predict(boost.hitters, newdata=hitters.clean[train,], n.trees=1000)
    train.mse <- mean((predictions - log(hitters.clean[train,]$Salary))^2)
    train.mses <- c(train.mses, train.mse)
}

plot(seq(0.1,1,0.1), train.mses, type="l")


# d.
# first, we get the test mse of boosting:
boost.model <- gbm(log(Salary)~.,
                   data=hitters.clean[train,],
                   distribution="gaussian",
                   n.trees=1000,
                   shrinkage=0.2)

test.predictions <- predict(boost.model, newdata=hitters.clean[!train,], n.trees=1000)
test.mse <- mean((test.predictions - log(hitters.clean[!train,]$Salary))^2)
test.mse

# e.
# ridge regression: 
library(glmnet)
grid <- 10^seq(10,-2,length=100)
x <- model.matrix(log(Salary)~.,hitters.clean)
y <- log(hitters.clean$Salary)
cv.out <- cv.glmnet(x[train,], y[train],alpha=0, lambda=grid)
bestlam <- cv.out$lambda.min
ridge.mod <- glmnet(x[train,], y[train], lambda=grid, alpha=0)
ridge.test.predictions <- predict(ridge.mod, s=bestlam, newx=x[-train,])
ridge.test.mse <- mean((ridge.test.predictions - y[-train])^2)
ridge.test.mse
# the test MSE for ridge regression shakes out slightly higher, at
# .38 v.s. .28 for boosting.

# I guess we have to do a second one according to the instructions.
lm.mod <- lm(log(Salary)~., data=hitters.clean, subset=train)
lm.test.predictions <- predict(lm.mod, newdata=hitters.clean[!train,])
lm.test.mse <- mean((lm.test.predictions - log(hitters.clean$Salary[!train]))^2)
lm.test.mse
# plain multiple regression is by far the worst of all. 
# we get a test MSE of .49

# f.
summary(boost.model)
# CAtBat, PutOuts and CHits seem to be the most important.

# g.
bagging.model <- randomForest(log(Salary)~., 
                              data=hitters.clean[train,], 
                              mtry=ncol(hitters.clean)-1)
bagging.pred <- predict(bagging.model, newdata=hitters.clean[!train,])
bagging.test.mse <- mean((bagging.pred - log(hitters.clean$Salary[!train]))^2)
# this is lowest of all at .23

# 11.
# a.
total.rows <- nrow(Caravan)
train <- rep(TRUE,total.rows)
train[1001:nrow(Caravan)] <- FALSE
# b.
library(gbm)
library(ISLR)
library(caret)

caravan.mod <- Caravan
caravan.mod$Purchase <- ifelse(caravan.mod$Purchase == "Yes", 1, 0)
Caravan.train <- caravan.mod[train,]
Caravan.test <- caravan.mod[!train,]
set.seed(342)
boost.caravan <- gbm(Purchase ~ ., data=Caravan.train, shrinkage=.01, 
                     n.trees=1000, distribution="bernoulli")
summary(boost.caravan)
# PPERSAUT, MKOOPKLA, and MOPLHOOG seem to be most important, in that order.
# c.
caravan.boost.pred <- ifelse(predict(boost.caravan, Caravan.test, n.trees=1000, type="response") > .2,1,0)

# confusion matrix
table(Caravan.test$Purchase, caravan.boost.pred)
34 / (34+137)
# roughly 20%

logistic.caravan <- glm(Purchase ~ ., data=Caravan.train, family="binomial")
logistic.predictions <- ifelse(predict(logistic.caravan, newdata=Caravan.test, type="response") > .2,1,0)
table(Caravan.test$Purchase, logistic.predictions)
58 / (58 + 350)
# for logistic regression, we only get ~14% of predicted purchasers actually making a purchase. 
