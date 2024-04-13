library(ISLR2)
library(MASS)
library(tidyverse)
source("regression_analysis.R")

DATASET <- Auto |> dplyr::select(-name)
DATASET |> View()

set.seed(1)
train <- DATASET |> slice_sample(prop = 0.8)
test <- DATASET |> anti_join(train)

analysis <- function(model, target = test$mpg) {
  c(
    R_squared = R2(target, predict(model, test)),
    # RSE = RSE(target, predict(model, test), length(coef(model)) - 1),
    # avg_target = mean(target),
    error_prop = RSE(target, predict(model, test), length(coef(model)) - 1) / mean(target)
  )
}

model1 <- lm(mpg ~ ., train)
summary(model1)
analysis(model1)

model_interactions1 <- lm(mpg ~ (.)^2, train)
summary(model_interactions1)
analysis(model_interactions1)

model_interactions2 <- model_interactions1 |> stepAIC()
summary(model_interactions2)
analysis(model_interactions2)

model2 <- stepAIC(model1)
summary(model2)
anova(model1, model2)
analysis(model1)
analysis(model2)

model3 <- update(model2, ~ . - acceleration + poly(acceleration, 2, raw = T))
anova(model2, model3)
analysis(model2)
analysis(model3)

summary(model3)
model4 <- update(model3, ~ . - weight + poly(weight, 2, raw = T))
analysis(model3)
analysis(model4)

summary(model4)
model5 <- update(model4, ~ . + log(year) - year + poly(year, 2, raw = T))
analysis(model4)
analysis(model5)
summary(model5) # best model

par(mfrow = c(1,2))
plot(model1, which = 1, main = "Model 1")
plot(model5, which = 1, main = "Model 5")

# mpg against horsepower
mpg_horsepower <- lm(mpg ~ horsepower, train)
plot(x = test$horsepower, y = test$mpg)
abline(mpg_horsepower)
plot(mpg_horsepower, which = 1)
summary(mpg_horsepower)
analysis(mpg_horsepower)

# scatterplot
pairs(DATASET)