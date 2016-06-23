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

