library(tidyverse)

train <- read_csv('data/train.csv')
test <- read_csv('data/test.csv')

train <- train |> janitor::clean_names()
test <- test |> janitor::clean_names()

train <- train |> dplyr::select(-c(name, ticket))
test <- test |> dplyr::select(-c(name, ticket))

library(stringr)
train <- train |> mutate(
  cabin = if_else(is.na(cabin), 'unknown', substring(cabin,1,1))
)
test <- test |> mutate(
  cabin = if_else(is.na(cabin), 'unknown', substring(cabin,1,1))
)

train$age[is.na(train$age)] <- mean(train$age, na.rm = T)
test$age[is.na(test$age)] <- mean(test$age, na.rm = T)

train |> filter(if_any(everything(), is.na))
test |> filter(if_any(everything(), is.na))

library(DescTools)
train$embarked[is.na(train$embarked)] <- Mode(train$embarked, na.rm = T)
# train$cabin[is.na(train$cabin)] <- Mode(train$cabin, na.rm = T)
# test$cabin[is.na(test$cabin)] <- Mode(test$cabin, na.rm = T)
test$fare[is.na(test$fare)] <- mean(test$fare, na.rm = T)
# train |> na.omit()

train <- train |> mutate(pclass = as.factor(pclass))
test <- test |> mutate(pclass = as.factor(pclass))

### DUMMY VARIABLES
library(fastDummies)
train_reg <- train |> dummy_cols() |> select(-c(sex, embarked, pclass, cabin))
test_reg <- test |> dummy_cols() |> select(-c(sex, embarked, pclass, cabin))

### SCALING
library(glmnet)
train_reg
test_reg <- test_reg |> mutate(
  cabin_T = rep(0, nrow(test_reg))
) |> relocate(cabin_T, .before = cabin_unknown)

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

# MATRIX FORM FOR FITTING IN GLMNET
X_train <- train_norm |> select(-c(passenger_id, survived))
y_train <- train_norm |> select(survived)
X_test <- test_norm |> select(-c(passenger_id))

X_train |> as.matrix() |> head()
y_train |> as.matrix() |> head()

set.seed(1)
ridge <- cv.glmnet(x = X_train |> as.matrix(), y = y_train |> as.matrix(),
                   type.measure="mse", alpha=0, family="binomial")
plot(ridge)
?cv.glmnet

### PREDICTION
ridge.predicted <- predict(ridge, ridge$lambda.1se,
                           newx = X_test |> as.matrix(),
                           type = 'response')
survived <- rep(0, nrow(X_test))
survived[ridge.predicted > 0.5] <- 1

# WRITING TO FILE
test['survived'] <- survived
test |>
  dplyr::select(passenger_id, survived) |> 
  rename(
    PassengerId = passenger_id,
    Survived = survived
  ) |>
  write_csv("data/ridge_predicted.csv")
