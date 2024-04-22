library(tidyverse)
library(ISLR2)
library(MASS)
library(boot)
source('regression_analysis.R')

# get index of minimal element in a vector
min_arg <- function(vector) {
  for (i in 1:length(vector)) {
    if (CV_error[i] == min(vector)) {
      min_arg_return <- i
      break
    }
  }
  min_arg_return
}

Auto <- Auto |> 
  dplyr::select(-name)

set.seed(1)

train <- sample(c(T,F), nrow(Auto), replace = T, prob = c(0.8, 0.2))

model1 <- glm(mpg ~ ., data = Auto[train,])
CV1 <- cv.glm(Auto[train,], model1, K = 10)$delta[1]

CV_error <- rep(0, 10)
for (i in 1:10) {
  CV_error[i] <- cv.glm(Auto[train,], 
                        glm(mpg ~ . - horsepower + poly(horsepower, i),
                            data = Auto[train,]),
                        K = 10)$delta[1]
}

min(CV_error)
min_arg(CV_error)

best_poly1 <- glm(mpg ~ . - horsepower + poly(horsepower, min_arg(CV_error)),
                  data = Auto[train,])
summary(best_poly1)

best_poly2 <- stepAIC(best_poly1)
cv.glm(Auto[train,], best_poly1, K = 10)$delta[1]
cv.glm(Auto[train,], best_poly2, K = 10)$delta[1]
plot(best_poly1, which = 1)
R2(Auto$mpg[!train], predict(best_poly1, Auto[!train,]))
