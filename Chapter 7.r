
x <- -3:3
y <- 1 + 1*x + -2*(x-1)^2*I(x>=1)
plot(x,y)

g <- function(x) {
    beta0 <- 1
    beta1 <- 1
    beta2 <- 3
    
    x1 <- (0 <= x & x <= 2) - (x - 1)*(1 <= x & x <= 2)
    x2 <- (x-3)*(3 <= x & x <= 4)+(4 < x & x <= 5)
    
    return(beta0 + beta1*x1 + beta2*x2)
}

x_vec <- seq(from=-2, to=2, by=.01)
y_vec <- g(x_vec)

plot(x_vec, y_vec)

library(ISLR)

colnames(Wage)

lmCrossVal <- function(dataset,x,y,modelfunc,cv,deg) {
    
    cv.vec <- sample(1:cv, nrow(dataset), replace=TRUE)
    results <- c()
      
    for(i in 1:cv){
        funcstr <- sprintf("%s ~ poly(%s, %s)", y, x, deg)
        model <- modelfunc(funcstr, data=dataset[cv.vec!=i,])
        cv.score <- mean((predict(model, newdata=dataset[cv.vec==i,]) - dataset[cv.vec==i,y])**2)
        results <- c(results, cv.score)
    }
    return(mean(results))
}
