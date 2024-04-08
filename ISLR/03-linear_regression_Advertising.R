library(tidyverse)
library(ISLR2)

# loading dataset into tidy and untidy versions
Advertising_tidy <- read_csv("data/Advertising.csv") |> 
  rename(
    X = `...1`
  ) |> 
  pivot_longer(
    cols = c(TV, radio, newspaper),
    names_to = "media",
    values_to = "budget"
  )

# pivot_wider undoes pivot_longer
Advertising <- Advertising_tidy |> 
  pivot_wider(
    names_from = media,
    values_from = budget
  )

# plotting
Advertising_tidy |> 
  ggplot(aes(x = budget, y = sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = 0) +
  labs(x = "Budget (10k USD)") +
  facet_wrap(~media, scales = "free")

# splitting
set.seed(1)
train <- Advertising |> 
  slice_sample(prop = 0.8)

test <- anti_join(Advertising, train)

avg_sales <- mean(test$sales)

# TV model simple linear regression
model_tv = lm(sales ~ TV, train)

source("analysis_tools.R")

SE_squared_intercept <- function(X, Y, Y_predicted, predictors) {
  RSE(Y, Y_predicted, predictors)^2 *
    (1/length(X) + mean(X)^2 / sum((X-mean(X))^2))
}

SE_squared_slope <- function(X, Y, Y_predicted, predictors) {
  RSE(Y, Y_predicted, predictors)^2 / sum((X-mean(X))^2)
}

RSE1 = RSE(test$sales, predict(model_tv, test), 1)
SE_squared_intercept(test$TV, test$sales, predict(model_tv, test), 1) 
SE_squared_slope(test$TV, test$sales, predict(model_tv, test), 1)
R2_1 <- R2(test$sales, predict(model_tv, test))

summary(model_tv)

ggplot(mapping = aes(x = test$TV, y = test$sales)) +
  geom_abline(slope = coef(model_tv)[2], intercept = coef(model_tv)[1]) +
  geom_point()

plot(x = test$TV, y = test$sales)
abline(model_tv)

# multiple variables
model_mv <- lm(sales ~ TV + radio + newspaper, train)
summary(model_mv)
cor(Advertising[,2:5])

RSE2 <- RSE(test$sales, predict(model_mv, test), 3)
R2_2 <- R2(test$sales, predict(model_mv, test))

# removing bad variables
library(MASS)
model_mv_optimized <- stepAIC(model_mv)
summary(model_mv_optimized)

RSE3 <- RSE(test$sales, predict(model_mv_optimized, test), 2)
R2_3 <- R2(test$sales, predict(model_mv_optimized,test))

test$predicted_sales <- predict(model_mv_optimized, test)
ggplot(test, aes(x = predicted_sales, y = sales)) +
  geom_abline() +
  geom_point()
plot(model_mv_optimized, which = 1)

# with interaction
model_interaction <- lm(sales ~ TV + radio + TV:radio, train)
RSE4 <- RSE(test$sales, predict(model_interaction, test), 3)
R2_4 <- R2(test$sales, predict(model_interaction, test))
error_prop_4 <- R2_4 / avg_sales

plot(model_interaction, which = 1)
cooks.distance(model_interaction)[127]
summary(model_interaction)

best_model <- model_interaction
