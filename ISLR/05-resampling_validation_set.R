library(tidyverse)
library(ISLR2)
library(MASS)

set.seed(1)

Auto <- Auto |> 
  dplyr::select(-name)

# validation set
train_index <- sample(c(T,F), nrow(Auto), replace = T, prob = c(0.8, 0.2))

model1 <- lm(mpg ~ ., data = Auto, subset = train_index)
summary(model1)
MSE1 <- (Auto$mpg - predict(model1, Auto))[-train_index]^2 |> mean()

model2 <- stepAIC(model1)
summary(model2)
MSE2 <- (Auto$mpg - predict(model2, Auto))[-train_index]^2 |> mean()

model3 <- update(model2, ~ . - cylinders)
summary(model3)
MSE3 <- (Auto$mpg - predict(model3, Auto))[-train_index]^2 |> mean()

model4 <- update(model3, ~ . - horsepower)
summary(model4)
MSE4 <- (Auto$mpg - predict(model4, Auto))[-train_index]^2 |> mean()

summary(model2)
model5 <- update(model2, ~ . - displacement + poly(displacement, 2))
summary(model5)
MSE5 <- (Auto$mpg - predict(model5, Auto))[-train_index]^2 |> mean()

model6 <- update(model5, ~ . - horsepower + poly(horsepower, 2))
summary(model6)
MSE6 <- (Auto$mpg - predict(model6, Auto))[-train_index]^2 |> mean()