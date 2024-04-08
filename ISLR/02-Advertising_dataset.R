library(ISLR2)
library(tidyverse)

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

# initial model
model1 <- lm(sales ~ TV+radio+newspaper, train)
model2 <- lm(sales ~ (TV+radio+newspaper)^2, train)
model3 <- lm(sales ~ (TV+radio+newspaper)^3, train)

# checking test error
mse1 <- (test$sales - predict(model1, test))^2 |> mean()
mse2 <- (test$sales - predict(model2, test))^2 |> mean()
mse3 <- (test$sales - predict(model3, test))^2 |> mean()

# select model2

# removing bad variables
library(MASS)
model2 |> summary()
stepAIC(model2) |> summary()
model2_step <- stepAIC(model2)

# comparing prediction to actual value
test$predicted_sales <- predict(model2_step, test)
test |> 
  ggplot(aes(x = sales, y = predicted_sales)) +
  geom_point() +
  geom_abline()

# reduced mse by 0.0074
mse2_step <- (test$sales - test$predicted_sales)^2 |> mean()
