library(tidyverse)
library(ISLR2)
library(MASS)
library(boot)

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
model1 <- glm(mpg ~ ., data = Auto)
CV1 <- cv.glm(Auto, model1, K = 10)$delta[1]

CV_error <- rep(0, 10)
for (i in 1:10) {
  CV_error[i] <- cv.glm(Auto, 
                        glm(mpg ~ . - horsepower + poly(horsepower, i),
                            data = Auto),
                        K = 10)$delta[1]
}

min(CV_error)
min_arg(CV_error)

best_poly1 <- glm(mpg ~ . - horsepower + poly(horsepower, min_arg(CV_error)),
                  data = Auto)
summary(best_poly1)

best_poly2 <- stepAIC(best_poly1)
cv.glm(Auto, best_poly1, K = 10)$delta[1]
cv.glm(Auto, best_poly2, K = 10)$delta[1]
plot(best_poly2, which = 1)
