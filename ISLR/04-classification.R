library(tidyverse)
library(ISLR2)
library(MASS)
source("analysis_tools.R")

set.seed(1)
train <- Default |> slice_sample(prop = 0.8)
test <- Default |> anti_join(train)

View(Default)

model1 <- glm(default ~ ., data = train, family = "binomial")
summary(model1)

model2 <- update(model1, ~ . -income)
summary(model2)

predict(model2, test, type = "response")
