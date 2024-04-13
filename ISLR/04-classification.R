library(tidyverse)
library(ISLR2)
library(MASS)

set.seed(1)
attach(Default)

train_index <- sample(c(T, F), nrow(Default), replace = T, prob = c(0.8, 0.2))
test <- Default[!train_index,]

model1 <- glm(default ~ ., data = Default, family = "binomial",
              subset = train_index)
summary(model1)

model2 <- update(model1, ~ . -income)
summary(model2)

model2_probs <- predict(model2, test, type = "response")
contrasts(default)
model2_pred <- rep("No", nrow(test))
model2_pred[model2_probs > 0.5] <- "Yes"

table(model2_pred, test$default)
mean(model2_pred == test$default)
error_rate <- mean(model2_pred != test$default)
