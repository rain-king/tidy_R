library(tidyverse)
library(MASS)
library(boot)
select <- dplyr::select

train <- read_csv('data/train.csv')
test <- read_csv('data/test.csv')

train <- train |> janitor::clean_names()
test <- test |> janitor::clean_names()

train <- train |> dplyr::select(-c(name, cabin, ticket))
test <- test |> dplyr::select(-c(name, cabin, ticket))

train$age[is.na(train$age)] <- mean(train$age, na.rm = T)
test$age[is.na(test$age)] <- mean(test$age, na.rm = T)

train |> filter(if_any(everything(), is.na))
test |> filter(if_any(everything(), is.na))

library(DescTools)
train$embarked[is.na(train$embarked)] <- Mode(train$embarked, na.rm = T)
test$fare[is.na(test$fare)] <- mean(test$fare, na.rm = T)
# train |> na.omit()

train <- train |> mutate(pclass = as.factor(pclass))
test <- test |> mutate(pclass = as.factor(pclass))

### DUMMY VARIABLES
library(fastDummies)
train_reg <- train |> dummy_cols() |> select(-c(sex, embarked, pclass))
test_reg <- test |> dummy_cols() |> select(-c(sex, embarked, pclass))

library(glmnet)
train_reg
train_norm <- train_reg |> mutate(
  age = scale(age)[,1],
  sib_sp = scale(sib_sp)[,1],
  parch = scale(parch)[,1],
  fare = scale(fare)[,1]
)
test_norm <- test_reg |> mutate(
  age = scale(age)[,1],
  sib_sp = scale(sib_sp)[,1],
  parch = scale(parch)[,1],
  fare = scale(fare)[,1]
)

X_train <- train_norm |> select(-c(passenger_id, survived))
y_train <- train_norm |> select(survived)
X_test <- test_norm |> select(-c(passenger_id))

X_train |> as.matrix()
y_train |> as.matrix()

ridge <- cv.glmnet(x = X_train |> as.matrix(), y = y_train |> as.matrix(),
                   type.measure="mse", alpha=0, family="binomial", nlambda=200)
plot(ridge)

### PREDICTION
ridge.predicted <- predict(ridge, ridge$lambda.1se,
                           new = X_test |> as.matrix(),
                           type = 'response')


survived <- rep(0, nrow(X_test))
survived[ridge.predicted > 0.5] <- 1

test['survived'] <- survived
test |>
  dplyr::select(passenger_id, survived) |> 
  rename(
    PassengerId = passenger_id,
    Survived = survived
  ) |>
  write_csv("data/ridge_predicted.csv")
