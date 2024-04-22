library(tidyverse)
library(ISLR2)
library(MASS)
source('regression_analysis.R')

set.seed(1)

Auto <- Auto |> 
  dplyr::select(-name)

# training+validation set index, split 0.7+0.15+0.15 
train_valid <- sample(c(T,F), nrow(Auto), replace = T, prob = c(0.85, 0.15))
test = !train_valid
# training set index 15/85 of 0.85 is 0.15
train <- sample(c(T,F), sum(train_valid),
                      replace = T, prob = c(70/85, 15/85))
validation <- !train

# train set
# View(Auto[train_valid,][train,])
# validation set
# View(Auto[train_valid,][validation,])

model1 <- lm(mpg ~ ., data = Auto[train_valid,], subset = train)
summary(model1)
R_squared1 <- R2(Auto$mpg[train_valid][validation],
                predict(model1, Auto)[train_valid][validation])

model2 <- stepAIC(model1)
summary(model2)
R_squared2 <- R2(Auto$mpg[train_valid][validation],
                predict(model2, Auto)[train_valid][validation])

model3 <- update(model2, ~ . - displacement + poly(displacement, 2, raw = T))
summary(model3)
R_squared3 <- R2(Auto$mpg[train_valid][validation],
                predict(model3, Auto)[train_valid][validation])

R_squared3_test <- R2(Auto$mpg[test], predict(model3, Auto)[test])
