library(tidyverse)
library(ISLR2)
library(MASS)

set.seed(1)

Auto <- Auto |> 
  dplyr::select(-name)

library(boot)

model1 <- glm(mpg ~ ., data = Auto)
CV1 <- cv.glm(Auto, model1)$delta[1]

model2 <- stepAIC(model1)
CV2 <- cv.glm(Auto, model2)$delta[1]
plot(model1, which = 1)

model3 <- update(model2, ~ . - horsepower + poly(horsepower, 2))
CV3 <- cv.glm(Auto, model3)$delta[1]
plot(model3, which = 1)

summary(model3)
model4 <- update(model3, ~ . - displacement + poly(displacement, 2))
CV4 <- cv.glm(Auto, model4)$delta[1]
plot(model4, which = 1)

summary(model4)
model5 <- update(model4, ~ . - weight + poly(weight, 2))
CV5 <- cv.glm(Auto, model5)$delta[1]
plot(model5, which = 1)

summary(model5)
model6 <- stepAIC(model5)
CV6 <- cv.glm(Auto, model6)$delta[1]
plot(model6, which = 1)

summary(model6)
