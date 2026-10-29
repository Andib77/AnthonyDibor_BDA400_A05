# ------------------------------------------------------------------
# Standard Deviation (stdev) - population formula (divide by n)
# BDA400 - Assignment 5 | Base R only (no external libraries)
# ------------------------------------------------------------------
# sigma = sqrt( sum((x_i - mean)^2) / n )
# returns: a single numeric value
# ------------------------------------------------------------------
stdev <- function(data) {
  if (length(data) == 0) stop("Data must contain at least one value")

  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)

  # Calculate the differences between the data points and the mean
  diff_values <- numeric(length(data))
  for (i in seq_along(data)) {
    diff_values[i] <- data[i] - mean_value
  }

  # Calculate the squared differences
  squared_diff <- numeric(length(diff_values))
  for (i in seq_along(diff_values)) {
    squared_diff[i] <- diff_values[i] * diff_values[i]
  }

  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)

  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}
