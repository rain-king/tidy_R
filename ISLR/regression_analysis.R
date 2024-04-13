# regression analysis
RSS <- function(vector, predicted_vector) {
  (vector - predicted_vector)^2 |> sum()
}

RSE <- function(vector, predicted_vector, predictors) {
  sqrt(RSS(vector, predicted_vector) / (length(vector) - predictors - 1))
}

TSS <- function(vector) {
  (vector - mean(vector))^2 |> sum()
}

R2 <- function(vector, predicted_vector) {
  (TSS(vector) - RSS(vector, predicted_vector)) / TSS(vector)
}

