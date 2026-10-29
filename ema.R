# ------------------------------------------------------------------
# Exponential Moving Average (EMA)
# BDA400 - Assignment 5 | Base R only (no external libraries)
# ------------------------------------------------------------------
# EMA(i) = (Price(i) - EMA(i-1)) * multiplier + EMA(i-1)
# multiplier (smoothing factor) = 2 / (period + 1)
# The first EMA value is seeded with the first data point.
# returns: numeric vector, same length as data
# ------------------------------------------------------------------
ema <- function(data, period) {
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # Initialize an empty array to store EMA values
  ema_values <- numeric(length(data))

  # Loop through the data array
  for (i in seq_along(data)) {
    if (i == 1) {
      # Calculate EMA for the first data point
      ema_values[i] <- data[i]
    } else {
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}
