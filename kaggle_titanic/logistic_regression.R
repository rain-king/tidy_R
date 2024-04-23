library(tidyverse)
library(MASS)
library(boot)

train <- read_csv('data/train.csv')
test <- read_csv('data/test.csv')

train <- train |> janitor::clean_names()
test <- test |> janitor::clean_names()

train <- train |> dplyr::select(-c(name, cabin, ticket))
test <- test |> dplyr::select(-c(name, cabin, ticket))

train$age[is.na(train$age)] <- mean(train$age, na.rm = T)
test$age[is.na(test$age)] <- mean(test$age, na.rm = T)

# train |> filter(if_any(everything(), is.na))

model1 <- glm(survived ~ ., data = train, family = "binomial")
summary(model1)
cv.glm(train |> na.omit(), model1, K = 10)$delta[1]

model2 <- stepAIC(model1) 
summary(model2)
cv.glm(train |> na.omit(), model2, K = 10)$delta[1]

model3 <- update(model2, ~ . - embarked)
summary(model3)
cv.glm(train, model3, K = 10)$delta[1]

predict(model2, test, type = 'response') |> head()
survived <- rep(0, nrow(test))
survived[predict(model2, test, type = 'response') > 0.5] <- 1

test['survived'] <- survived
test |>
  dplyr::select(passenger_id, survived) |> 
  rename(
    PassengerId = passenger_id,
    Survived = survived
  ) |>
  write_csv("data/lr_predicted.csv")
