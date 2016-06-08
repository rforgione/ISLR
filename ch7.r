library(ISLR)
set.seed(1)

dim(Wage)

lmCrossVal <- function(data,x,y,modelfunc,cv,deg) {
  
  cv.vec <- sample(1:cv, nrow(data), replace=TRUE)
  results <- c()
  
  for(i in 1:cv){
    funcstr <- sprintf("%s ~ poly(%s, %s)", y, x, deg)
    model <- modelfunc(funcstr, data[cv.vec != i,])
    predictions <- predict(model, data[cv.vec == i,])
    cv.score <- mean((predictions - data[cv.vec == i,y])**2)
    results <- c(results, cv.score)
  }
  return(mean(results))
}

print(lmCrossVal(Wage, "wage", "age", lm, 10, 1))
print(lmCrossVal(Wage, "wage", "age", lm, 10, 2))
print(lmCrossVal(Wage, "wage", "age", lm, 10, 3))
print(lmCrossVal(Wage, "wage", "age", lm, 10, 4))
print(lmCrossVal(Wage, "wage", "age", lm, 10, 5))
print(lmCrossVal(Wage, "wage", "age", lm, 10, 6))
print(lmCrossVal(Wage, "wage", "age", lm, 10, 7))

=======
library(boot)

set.seed(1)

errors <- c()

for (i in 1:10) {
    fit <- glm(wage ~ poly(age,i), data=Wage)
    cv.error <- cv.glm(Wage, fit, K=10)$delta[2]
    errors <- c(errors, cv.error)

    print(cv.error)
}

cvs = seq(1:10)

plot(cvs, errors, type="l")

min.score <- min(errors)

sd.range <- sd(errors)*.2

reasonable <- errors[errors < min.score + sd.range][1]

best.degree <- which(errors == reasonable)

best.degree.score <- errors[best.degree]

best.fit <- glm(wage ~ poly(age,best.degree), data=Wage)

lm.fit.1 <- lm(wage~poly(age,1), data=Wage.clean)
lm.fit.2 <- lm(wage~poly(age,2), data=Wage.clean)
lm.fit.3 <- lm(wage~poly(age,3), data=Wage.clean)
lm.fit.4 <- lm(wage~poly(age,4), data=Wage.clean)
lm.fit.5 <- lm(wage~poly(age,5), data=Wage.clean)
lm.fit.6 <- lm(wage~poly(age,6), data=Wage.clean)
lm.fit.7 <- lm(wage~poly(age,7), data=Wage.clean)

anova(lm.fit.1, lm.fit.2, lm.fit.3, lm.fit.4, lm.fit.5, lm.fit.6, lm.fit.7)

age.axis = seq(1:100)
wage.axis <- predict(best.fit, newdata=list(age = age.axis))

plot(wage~age, data=Wage, col="darkgrey")
lines(age.axis, wage.axis, col="blue", lwd=2)

errors.step <- c()

for(i in 2:10) {
    Wage$age.new <- cut(Wage$age,i)
    fit <- glm(wage~age.new, data=Wage) 
    errors.step <- c(errors.step, cv.glm(Wage, fit, K=10)$delta[2])
}

min.score <- min(errors.step)
sd.score <- sd(errors.step)

min.reasonable <- errors.step[which(errors.step == errors.step[errors.step < min.score + .2*sd.score][1])]

min.reasonable.degree <- which(errors.step == min.reasonable)

Wage$age.new <- cut(Wage$age,min.reasonable.degree)

best.fit.step <- glm(wage~cut(age, min.reasonable.degree), data=Wage)

plot(2:10, errors.step, type="l", pch=20, lwd=2)

predicted.wage.step <- predict(best.fit.step, list(age=Wage$age))

plot(Wage$age, predicted.wage.step)

Wage$age.new <- cut(Wage$age,min.reasonable.degree)

lm.fit <- glm(wage~cut(age,min.reasonable.degree), data=Wage)
age.grid <- 18:80
lm.pred <- predict(lm.fit, data.frame(age=age.grid))

plot(wage~age, data=Wage, col="darkgrey")
lines(18:80, lm.pred, col="red", lwd=2)

### 
# Question 7
###

cv.scores <- c()

for(i in 1:10) {
    glm.fit.7 <- glm(wage~poly(maritl,i), data=Wage)
    cv.scores <- c(cv.scores, cv.glm(glm.fit.7, Wage, K=10)$delta[2])    
}

reasonable.score <- cv.scores[cv.scores < min(cv.scores) + .2*sd(cv.scores)][1]

best.degree <- which(cv.scores == reasonable.score)

best.model <- glm(wage~poly(maritl,i), data=Wage)

###
# Question 8
###

cv.scores <- c()

for(i in 1:10) {
    fit <- glm(mpg~poly(horsepower,i), data=Auto)
    cv.scores <- c(cv.scores, cv.glm(Auto, fit, K=10)$delta[2])
}

reasonable.score <- cv.scores[cv.scores < min(cv.scores) + .2*sd(cv.scores)][1]
best.degree <- which(cv.scores == reasonable.score)
best.model <- glm(mpg~poly(horsepower, best.degree), data=Auto)

plot(mpg~horsepower, data=Auto, pch=20, col="darkgrey")
lines(50:200, predict(best.model, data.frame(horsepower=c(50:200))), col="blue", pch=20)


cv.scores <- c()

for(i in 2:10) {
    fit <- glm(mpg~cut(horsepower,i), data=Auto)
    cv.scores <- c(cv.scores, cv.glm(Auto, fit))
}

chosen.score <- which(cv.scores == min(cv.scores[cv.scores < min(cv.scores) + .2*sd(cv.scores)]))
chosen.model <- glm(mpg~cut(horsepower,chosen.score), data=Auto)

plot(mpg~horsepower, data=Auto, pch=20, col="darkgrey")
hp.range <- range(Auto$horsepower)
hp.grid <- seq(hp.range[1], hp.range[2])
lines(hp.grid, predict(chosen.model, data.frame(horsepower=hp.grid)), col="blue", lwd=5)

###
# Question 9
###

library(MASS)

# a.
fit <- glm(nox~poly(dis,3), data=Boston)

# Regression output:
summary(fit)

# b.

scores <- c()

for(i in 1:10) {
    fit <- glm(nox~poly(dis,i), data=Boston)
    scores <- c(scores, sum(fit$residuals^2))
}

scores
plot(1:10, scores, type="l", lwd=2)

# c. 
cv.scores <- c()

for(i in 1:10) {
    fit <- glm(nox~poly(dis,i), data=Boston)
    cv.scores <- c(cv.scores, cv.glm(Boston, fit, K=10)$delta[2])
}

best.degree <- which(cv.scores == cv.scores[cv.scores < min(cv.scores) + sd(cv.scores)*.2][1])
best.fit <- glm(nox~poly(dis,best.degree), data=Boston)

# d.
library(splines)
fit <- glm(nox~bs(dis,4), data=Boston)
summary(fit)

# Chose the knots by allowing the software to pick them uniformly
# across the data.

dis.range <- range(Boston$dis)
dis.grid <- seq(dis.range[1], dis.range[2])
nox.predicted <- predict(fit, newdata=data.frame(dis=dis.grid))
plot(nox~dis, data=Boston, col="darkgrey", pch=20)
lines(dis.grid, nox.predicted, type="l", col="red", lwd=3)

# e.
rss <- c()

for(i in 1:10) {
    fit <- glm(nox~bs(dis,df=i), data=Boston)
    rss <- c(rss, sum(fit$residuals^2))
}

plot(1:10, rss, type="l", col="darkgrey", lwd=3)

# RSS drops of drastically at df=5 and then slowly
# continues to drop off after that.

# f.
cv.scores <- c()
for(i in 1:10) {
    fit <- glm(nox~bs(dis,df=i), data=Boston)
    cv.scores <- c(cv.scores, cv.glm(Boston, fit, K=10)$delta[2])
}

reasonable <- cv.scores[cv.scores < min(cv.scores) + .2*sd(cv.scores)][1]

degree <- which(cv.scores == reasonable)
optimal.score <- cv.scores[degree]

# 5 df gives us a reasonably good CV score while also keeping
# the model relatively simple. Note: a cubic spline with
# 5 df has one knot -- the 5 degrees of freedom are intercept,
# X, X^2, X3, K. Unless you specify a different degree, the bs
# function fits cubic splines.

best.model <- glm(nox~bs(dis,df=degree), data=Boston)

# 10.
# a.
n <- nrow(College)

train.pct <- .75
test.pct <- 1 - train.pct

test.samples <- sample(n,round(test.pct*n),replace=FALSE)

train.set <- College[-test.samples,]
test.set <- College[test.samples,]

regfit.fwd <- regsubsets(Outstate~.,data=College, method="forward", nvmax=18)

test.mat <- model.matrix(Outstate~.,data=test.set)

errors <- c()

for (i in 1:17) {
    coefi <- coef(regfit.fwd, id=i)
    pred <- test.mat[,names(coefi)]%*%coefi
    errors <- c(errors,mean((test.set$Outstate - pred)^2))
}

plot(1:17, errors, type="l", col="darkgrey", lwd=3)

reasonable.error <- errors[errors < min(errors) + .2*sd(errors)][1]

reasonable.model <- which(errors == reasonable.error)

reasonable.model

included.vars <- names(coef(regfit.fwd,id=reasonable.model))[-1]

# We choose the model with 8 variables because it is a pretty combination
# trade-off of simplicity and explanation of the variance. 

# b.

gam.fit <- gam(Outstate~ns(Room.Board, df=4)+ns(Personal,df=4)+ns(PhD, df=4)+ns(Terminal, df=4)+ns(perc.alumni, df=4)+ns(Expend, df=4)+ns(Grad.Rate, df=4)+Private, data=train.set)

par(mfrow=c(4,4))
plot(gam.fit)


# c. 

test.predictions <- predict(gam.fit, newdata=test.set)

mse <- mean((test.predictions - test.set$Outstate)^2)

gam.tss = mean((test.set$Outstate - mean(test.set$Outstate))^2)

r_2 = 1 - mse/gam.tss

# d.
summary(gam.fit)

# Yes, there appears to be strong evidence of non-linear relationships for every
# variable except Terminal.

# 11.
# a.
set.seed(1)
g = data.frame(y=rnorm(100), x1=rnorm(100), x2=rnorm(100))


# b.
beta1 <- 2

# c.
a <- g$y-(beta1*g$x1)
beta2 <- lm(a~g$x2)$coef[2]

# d.
a <- g$y-(beta2*g$x2)
beta1 <- lm(a~g$x1)$coef[2]

# e.
coefs <- data.frame()
for(i in 1:10) {
    if (i == 1) {
        beta1 <- 100
    }
    a <- g$y-(beta1*g$x1)
    beta2 <- lm(a~g$x2)$coef[2]
    b <- g$y-(beta2*g$x2)
    beta1 <- lm(b~g$x1)$coef[2]
    beta0 <- lm(b~g$x1)$coef[1]
    
    print(sprintf("Iteration %s", i))
    print(sprintf("beta0 is %s", beta0))
    print(sprintf("beta1 is %s", beta1))
    print(sprintf("beta2 is %s", beta2))
    coefs <- rbind(coefs, c(beta0, beta1, beta2))
}

colnames(coefs) <- c("beta0", "beta1", "beta2")

plot(1:1000, coefs$beta0, col="red", type="l")
lines(1:1000, coefs$beta1, col="green")
lines(1:1000, coefs$beta2, col="blue")

# f.
lmfit <- lm(y~x1+x2, data=g)
summary(lmfit)
# the estimates are very close.
abline(lmfit$coef[2],0, col="purple", lwd="2")
abline(lmfit$coef[3],0, col="orange", lwd="2")
abline(lmfit$coef[1],0, col="gray", lwd="2")
# confirms that the fitted coefficients are extremely close.

# g.
# only about four iterations.

# 12.
h <- matrix(ncol=100, nrow=500)
for(i in 1:100) {
    h[,i] <- rnorm(500)
}
y <- rnorm(500)
colnames(h) <- sprintf("x%s", 1:100)
coefs <- data.frame()

current.coefs <- rnorm(100)

for(i in 1:10) {
    for (i in 1:100) {
        a <- y - h %*% current.coefs + h[,i] * current.coefs[i]
        current.coefs[i] <- lm(a~h[,i])$coef[2]
    }
    coefs <- rbind(coefs, current.coefs)
}
colnames(coefs) <- sprintf("x%s", 1:100)

plot(1:10, coefs$x1, type="l", col="blue", lwd=2, ylim=c(-.1,.1))
lines(1:10, coefs$x2, type="l", col="red", lwd=2)
lines(1:10, coefs$x3, type="l", col="green", lwd=2)
lines(1:10, coefs$x4, type="l", col="purple", lwd=2)
lines(1:10, coefs$x5, type="l", col="orange", lwd=2)

# these all seem to stabilize around 4-5 iterations.
# let's try an actual lm.

h.f <- data.frame(h)
h.f$y <- y
summary(lm(y~., h.f))

current.coefs[1]
current.coefs[2]
current.coefs[3]
current.coefs[99]
current.coefs[100]

# we level out at the same values as the lm function.
