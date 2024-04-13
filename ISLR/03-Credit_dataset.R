library(tidyverse)
library(ISLR2)
source("analysis_tools.R")

set.seed(1)
train <- Credit |> slice_sample(prop = 0.8)
test <- Credit |> anti_join(train)

avg_test_balance <- mean(test$Balance)

model1 <- lm(Balance ~ ., train)
R2_1 <- R2(test$Balance, predict(model1, test))
RSE_1 <- RSE(test$Balance, predict(model1, test), 11)
summary(model1)

library(MASS)
model2 <- stepAIC(model1)
R2_2 <- R2(test$Balance, predict(model2, test))
RSE_2 <- RSE(test$Balance, predict(model2, test), length(coef(model2)) - 1)
summary(model2)
plot(model2, which = 1)

model3 <- stepAIC(lm(Balance ~
  (Income + Limit + Cards + Age + Own + Student)^2, train))
R2_3 <- R2(test$Balance, predict(model3, test))
summary(model3)
RSE_3 <- RSE(test$Balance, predict(model3, test), length(coef(model3)) - 1)
plot(model3, which = 1, main = "Model 3.0")

model32 <- stepAIC(lm(Balance ~ I(log(Limit)) +
                        poly(Income, 5) + poly(Limit, 5) + poly(Cards, 5) +
                        Age + Own + Student + 
                        Income:Limit + Income:Own + Income:Student + Limit:Age + 
                        Limit:Own + Limit:Student + Cards:Own,
                      train))
summary(model32)
R2_32 <- R2(test$Balance, predict(model32, test))
RSE_32 <- RSE(test$Balance, predict(model32, test), length(coef(model32)) - 1)
plot(model32, which = 1, main = "Model 3.2")
error_prop_32 <- RSE_32 / avg_test_balance


model4 <- stepAIC(lm(Balance ~ (.)^2, train))
R2_4 <- R2(test$Balance, predict(model4, test))
summary(model4)
RSE_4 <- RSE(test$Balance, predict(model4, test), length(coef(model4)) - 1)
plot(model4, which = 1, main = "Model 4.0")

model42 <- stepAIC(lm(Balance ~
  poly(Income, 5) + poly(Limit, 5) + poly(Cards, 5) + poly(Rating, 5) +
    poly(Age, 5) +
  Education + Own + Student + Married + Region + Income:Rating + 
  Income:Cards + Income:Age + Income:Education + Income:Own + 
  Income:Student + Income:Married + Income:Region + Limit:Rating + 
  Limit:Own + Limit:Student + Rating:Age + Cards:Own + Education:Married + 
  Education:Region + Own:Married,
                      train))
summary(model42)
R2_42 <- R2(test$Balance, predict(model42, test))
RSE_42 <- RSE(test$Balance, predict(model42, test), length(coef(model42)) - 1)
plot(model42, which = 1, main = "Model 4.2")


best_model <- model32
