library(tidyverse)
library(gbm)

train <- read_csv('data/train.csv')
test <- read_csv('data/test.csv')

train <- train |> janitor::clean_names()
test <- test |> janitor::clean_names()

train <- train |> dplyr::select(-c(name, cabin, ticket))
test <- test |> dplyr::select(-c(name, cabin, ticket))

train <- train |> mutate(
  sex = as.factor(sex),
  embarked = as.factor(embarked)
)

# imputation not needed for gbm
# train$age[is.na(train$age)] <- mean(train$age, na.rm = T)
# test$age[is.na(test$age)] <- mean(test$age, na.rm = T)
# train <- train |> na.omit()

set.seed(1)
gbm.fit1 <- gbm(
  formula = survived ~ . - passenger_id,
  distribution = 'bernoulli',
  data = train,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.1,
  cv.folds = 10,
  n.cores = 2,
  verbose = F
)
gbm.fit1
lowest_error1 <- min(gbm.fit1$cv.error)

gbm.fit2 <- gbm(
  formula = survived ~ . - passenger_id,
  distribution = 'bernoulli',
  data = train,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.01,
  cv.folds = 10,
  n.cores = 2,
  verbose = F
)
gbm.fit2
lowest_error2 <- min(gbm.fit2$cv.error)

gbm.fit3 <- gbm(
  formula = survived ~ . - passenger_id,
  distribution = 'bernoulli',
  data = train,
  n.trees = 5000,
  interaction.depth = 1,
  shrinkage = 0.1,
  cv.folds = 10,
  n.cores = 2,
  verbose = F
)
gbm.fit3
lowest_error3 <- min(gbm.fit3$cv.error)

gbm.fit4 <- gbm(
  formula = survived ~ . - passenger_id,
  distribution = 'bernoulli',
  data = train,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 10,
  n.cores = 2,
  verbose = F
)
gbm.fit4
lowest_error4 <- min(gbm.fit4$cv.error)

gbm.fit5 <- gbm(
  formula = survived ~ . - passenger_id,
  distribution = 'bernoulli',
  data = train,
  n.trees = 500,
  interaction.depth = 2,
  shrinkage = 1,
  cv.folds = 10,
  n.cores = 2,
  verbose = F
)
gbm.fit5
lowest_error5 <- min(gbm.fit5$cv.error)

summary(
  gbm.fit1, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

best_n_trees <- gbm.perf(gbm.fit1, method = "cv")

predicted_rate <- predict(gbm.fit1,
                     newdata = test,
                     n.trees = best_n_trees,
                     type = 'response')

survived <- rep(0, length(predicted_rate))
survived[predicted_rate > 0.5] <- 1

test['survived'] <- survived
test |>
  dplyr::select(passenger_id, survived) |> 
  rename(
    PassengerId = passenger_id,
    Survived = survived
  ) |>
  write_csv("data/gbm_predicted.csv")

