# 9.6.1

set.seed(1)

x <- matrix(rnorm(20*2), ncol=2)

y=c(rep(-1,10), rep(1,10))

x[y==1,] <- x[y==1,] + 1

plot(x, col=(3-y))

dat <- data.frame(x=x, y=as.factor(y))
library(e1071)
svmfit <- svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)

plot(svmfit, dat)

svmfit$index

summary(svmfit)

svmfit=svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit, dat)

set.seed(1)
tune.out <- tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

bestmod <- tune.out$best.model

summary(bestmod)

xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)

svmfit=svm(y~.,data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred, truth=testdat$y)

x[y==1,]=x[y==1,]+0.5
plot(x,col=(y+5)/2,pch=19)
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit,dat)
# the margin is pretty narrow -- the support vectors are very close together
# this is a signal that the model will probably do poorly on test data
svmfit <- svm(y~.,data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit,dat)
# much wider margin with more support vectors
# likely to perform better on test data

# 9.6.2
set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,] <- x[1:100,]+2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x=x, y=as.factor(y))
plot(x,col=y)
# essentially we're taking 1:100 and pushing them 
# right two, taking 101:150 and pushing them left 2,
# leaving 150-200 in the middle, and making 150-200 a diff
# class such that the decision boundary is a circle in the middle.
train <- sample(200,100)
svmfit <- svm(y~.,data=dat[train,], kernel="radial", gamma=1, cost=1)
plot(svmfit, dat[train,])
summary(svmfit)
svmfit <- svm(y~.,data=dat[train,], kernel="radial", gamma=1, cost=1e5)
plot(svmfit,dat[train,])

set.seed(1)
tune.out <- tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),
                                                                          gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train,"y"], pred=predict(tune.out$best.model, newx=dat[-train,]))
# 39% misclassification rate

# 9.6.3
library(ROCR)

rocplot <- function(pred, truth, ...) {
    predob <- prediction(pred,truth)
    perf <- performance(predob, "tpr", "fpr")
    plot(perf, ...)
}

svmfit.opt <- svm(y~., data=dat[train,], 
                  kernel="radial", gamma=2, cost=1, decision.values=T)
fitted <- attributes(predict(svmfit.opt, dat[train,], decision.values=TRUE))$decision.values

par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"], main="Training Data")

# 9.6.4 SVM with Multiple Classes
set.seed(1)
x=rbind(x,matrix(rnorm(50*2),ncol=2))
y <- c(y,rep(0,50))
x[y==0,2] <- x[y==0,2] + 2
dat <- data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))
svmfit <- svm(y~.,data=dat, kernel="radial", cost=10, gamma=1)

# 9.6.5 Application to Gene Expression Data
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)
dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out <- svm(y~.,data=dat, kernel="linear", cost=10)
summary(out)
table(out$fitted, dat$y)

dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

# 9.7 Exercises

# 1.
par(mfrow=c(1,1))
# a. 
ax1 <- seq(-10,10)
ax2 <- 1 + 3*ax1
plot(ax1,ax2, type="l", col="red")
text(c(0),c(-20), "greater than 0", col="red")
text(c(0),c(20), "less than 0", col="red")
# b.
bx2 <- seq(-10,10)
bx1 <- 2 - 2*bx2
lines(bx1,bx2,type="l", col="blue")
text(c(0),c(-15), "less than 0", col="blue")
text(c(0),c(15), "greater than 0", col="blue")

# 2. 
# a.
x1 <- seq(-10:10)
x2 <- seq(-10:10)

radius = 2
plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 9), asp = 1, xlab = "X1", 
     ylab = "X2")
symbols(c(-1), c(2), circles = c(radius), add = TRUE, inches = FALSE)

# b.
text(x = -1, y = 5, labels = "greater than 4")
text(x = -1, y = 2, labels = "less than 4")

# c. 
decision.func <- function(x1,x2) {
    if ((1+x1)^2 + (2 - x2)^2 > 4) {
        "BLUE"
    } else {
        "RED"
    }
}
decision.func(0,0)
decision.func(-1,1)
decision.func(2,2)
decision.func(3,8)

points(c(0, -1, 2, 3), c(0, 1, 2, 8), col = c("blue", "red", "blue", "blue"), 
     type = "p", asp = 1, xlab = "X1", ylab = "X2")

# d.
# did this on paper -- basically just expand out the polynomial

# 3.
# a.
plot(3,4, col="red", xlim=c(0,5), ylim=c(0,5))
points(2,2,col="red")
points(4,4,col="red")
points(1,4,col="red")
points(2,1,col="blue")
points(4,3,col="blue")
points(4,1,col="blue")

# we can use point-slope to figure this out
# b.
abline(-.5,1)

# c.
# x2 = 0.5 + x1
# 0 = 0.5 + x1 - x2
# b0 = 0.5, b1 = 1, b2 = -1

# d.
abline(-1, 1, lty = 2)
abline(0, 1, lty = 2)
arrows(2,1,2,1.5)
arrows(2,2,2,1.5)
arrows(4,3,4,3.5)
arrows(4,4,4,3.5)

# e. A slight movement in point #7 would not affect the maximal margin
# hyperplane because it depends only on the support vectors. 
points(2,3, col="blue")

# 4.
x1 <- rnorm(100)
x2 <- rnorm(100)

y <- rep("blue",100)
rad <- .8
y[(x1 < rad) & (x1 > -rad) & (x2 < rad) & (x2 > -rad)] <- "red"

space <- .2

x1[x1 < -rad] <- x1[x1 < -rad] - space
x1[x1 > rad] <- x1[x1 > rad] + space
x2[x2 < -rad] <- x2[x2 < -rad] - space
x2[x2 > rad] <- x2[x2 > rad] + space

df <- data.frame(x1,x2,y)

test <- rep(FALSE,100)
test[51:100] <- TRUE

with(df, plot(x1,x2,col=y))

svm.svc <- svm(y~., data=df[!test,], kernel="linear", cost=10)
# test table
table(predict(svm.svc, newdata=df[test,], type="class"), df[test,]$y)
# test error
13/(38+13)
# 26% error rate
# train table
table(predict(svm.svc, newdata=df[!test,]), df[!test,]$y)
9/40
# ~30% train error

svm.rbf <- svm(y~., data=df[!test,], kernel="radial", cost=10)
# train table
table(predict(svm.rbf, newdata=df[!test,]), df[!test,]$y)
# train error rate is 
# 0
plot(svm.rbf,df)

# test table
table(predict(svm.rbf, newdata=df[test,]), df[test,]$y)
# test error rate is 
# 0

# no train or test errors for radial kernel.

# 5.
# a.
x1 <- runif(500)-0.5
x2 <- runif(500)-0.5
y <- 1*(x1^2 - x2^2 > 0)

df <- data.frame(x1,x2,y)

# b.
# add 1 because R uses zero-indexed arrays -- if y is 0 or 1,
# we need to add 1 to make it 1-indexed for access to the color array
# that we pass
plot(x1,x2,col=c("red","blue")[y+1])

test <- rep(FALSE,500)
set.seed(1)
test[sample(500,100)] <- TRUE

# c.
lg.mod <- glm(y~x1+x2,data=df,family="binomial")

# d.
pred.responses <- predict(lg.mod,newdata=df, type="response")
pred.labels <- ifelse(pred.responses > .5,1,0)

plot(df$x1,df$x2,col=c("red","blue")[pred.labels + 1])

# e.
lg.non.linear <- glm(y~poly(x1,2)+poly(x2,2)+I(x1*x2),data=df,family="binomial")

# f.
pred.responses.non.linear <- predict(lg.non.linear,newdata=df, type="response")
pred.labels.non.linear <- ifelse(pred.responses.non.linear > .5,1,0)
plot(df$x1,df$x2,col=c("red","blue")[pred.labels.non.linear + 1])
# the decision boundary looks like the actual one.

# g.
svc <- svm(y~.,data=df,kernel="linear", cost=10)
pred.responses.svc <- predict(svc,newdata=df, type="response")
pred.labels.svc <- ifelse(pred.responses.svc > .5,1,0)
plot(df$x1,df$x2,col=c("red","blue")[pred.labels.svc + 1])

svm.rad <- svm(y~.,data=df,kernel="radial", cost=10)
pred.responses.svm <- predict(svm.rad,newdata=df, type="response")
pred.labels.svm <- ifelse(pred.responses.svm > .5,1,0)
plot(df$x1,df$x2,col=c("red","blue")[pred.labels.svm + 1])

# h. The non-linear svm also shows the true classification boundary.

# 9
# (a)
a <- rnorm(100)
b <- rnorm(100)
data.mat <- data.eframe(a,b)
plot(data.mat$a, data.mat$b)
data.mat[data.mat$b < data.mat$a,'a'] <- data.mat[data.mat$b < data.mat$a,'a'] + .1
data.mat$class <- ifelse(data.mat$b < data.mat$a,1,0)
cols <- c("red", "blue")
plot(data.mat$a, data.mat$b, col=cols[data.mat$class + 1])

# (b)
tune.out <- tune(svm, class~., data=data.mat, kernel="linear", ranges=list(cost=c(0.1,1,10,100,1000)))
svc <- svm(class~., data=data.mat, kernel="linear", )
summary(tune.out)
# A little over 9% of the examples are misclassified. This goes up as the cost goes up.

# (c)