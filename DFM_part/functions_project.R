# Functions used for DFM forecast of House prices in USA

calculate_adjusted_r_squared <- function(y_true, y_pred, k) {
  # y_true is a vector with the target
  # y_pred is the predicted vector
  # k corresponds to the number of regressors
  
  # Calculate the residuals
  residuals <- y_true - y_pred
  
  # Calculate the sum of squares
  ss_residuals <- sum(residuals^2)
  ss_total <- sum((y_true - mean(y_true))^2)
  
  # Calculate the number of observations
  n <- length(y_true)

  # Calculate Adjusted R-squared
  adjusted_r_squared <- 1 - (ss_residuals / ss_total)*((n - 1) / (n - k - 1))

  return(adjusted_r_squared)
}
