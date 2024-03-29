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

Advertising_tidy |> 
  pivot_wider(
    names_from = media,
    values_from = budget
  )

Advertising <- read_csv("data/Advertising.csv")

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
model <- lm(sales ~ TV+radio+newspaper, train)
summary(model)

rmse = (test$sales-predict(model, test))^2 |> mean() |> sqrt()
rmse

# removing bad variables
library(MASS)
model2 <- stepAIC(model)
summary(model2)

# comparing prediction to actual value
test$predicted_sales <- predict(model2, test)
test |> 
  ggplot(aes(x = sales, y = predicted_sales)) +
  geom_point()+
  geom_abline()

mean(model2$residuals^2) |> sqrt()

rmse2 = (test$sales-test$predicted_sales)^2 |> mean() |> sqrt()
rmse2
