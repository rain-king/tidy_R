library(tidyverse)
library(ISLR2)
library(MASS)

set.seed(1)

Auto <- Auto |> 
  dplyr::select(-name)

library(boot)

model1 <- glm(mpg ~ ., data = Auto)
CV1 <- cv.glm(Auto, model1, K = 10)$delta[1]
