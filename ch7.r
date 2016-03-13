library(ISLR)
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

plot(1:10, scores, type="l", lwd=2)

# c. 
cv.scores <- c()

for(i in 1:10) {
    fit <- glm(nox~poly(dis,i), data=Boston)
    cv.scores <- c(cv.scores, cv.glm(Boston, fit, K=10)$delta[2])
}

best.degree <- which(cv.scores == cv.scores[cv.scores < min(cv.scores) + sd(cv.scores)*.2][1])
best.fit <- glm(nox~poly(dis,best.degree), data=Boston)

