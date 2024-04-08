library(tidyverse)
library(ISLR2)
library(MASS)
source("analysis_tools.R")

set.seed(4)
train <- Boston |> slice_sample(prop = 0.8)
test <- Boston |> anti_join(train)

analysis <- function(model, target = test$medv) {
  c(
    R_squared = R2(target, predict(model, test)),
    # RSE = RSE(target, predict(model, test), length(coef(model)) - 1),
    # avg_target = mean(target),
    error_prop = RSE(target, predict(model, test), length(coef(model)) - 1) / mean(target)
  )
}

model1 <- lm(medv ~ ., train)
summary(model1)
plot(model1, which = 1)

model1 <- stepAIC(model1)

model2 <- update(model1, ~ (.)^2)
summary(model2)
anova(model1, model2)
analysis(model1)
analysis(model2)

par(mfrow = c(1,2))
plot(model1, which = 1)
plot(model2, which = 1)

plot(model1, which = 5)
plot(model2, which = 5)

summary(model1)
model3 <- update(model1, ~ . - lstat + poly(lstat, 5))
anova(model1, model3)
analysis(model1)
analysis(model3)

summary(model3) # - (crim, nox,)
model4 <- update(model3, ~ . - rm + poly(rm,2))
anova(model3, model4)
analysis(model3)
analysis(model4)

plot(model1, which = 1)
plot(model4, which = 1)

summary(model4) # - (crim, nox,)
model5 <- update(model4, ~ . - zn)
anova(model4, model5)
analysis(model4)
analysis(model5)

summary(model5) # - chas crim nox dis rad tax
model6 <- update(model5, ~ . - ptratio + poly(ptratio, 2))
anova(model5,model6)
analysis(model5)
analysis(model6)

plot(model1, which = 1, main = "Model 1")
plot(model6, which = 1, main = "Model 6")

summary(model6) # best model