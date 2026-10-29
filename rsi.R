# ------------------------------------------------------------------
# Relative Strength Index (RSI) - Wilder's smoothing
# BDA400 - Assignment 5 | Base R only (no external libraries)
# ------------------------------------------------------------------
# RSI = 100 - 100 / (1 + RS),  RS = average gain / average loss
# returns: numeric vector, same length as data. The first 'period'
#          values are NA because a full period of price changes is
#          needed before the first RSI can be computed.
# ------------------------------------------------------------------
rsi <- function(data, period) {
  n <- length(data)
  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, n)

  if (n <= period) {
    warning("Not enough data: RSI needs more than 'period' data points; returning all NA")
    return(rsi_values)
  }

  # Calculate the differences between consecutive data points
  diff_values <- data[2:n] - data[1:(n - 1)]

  # Initialize two vectors to store the gains and losses
  gains  <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses (a gain day has 0 loss and vice versa)
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Helper: RSI from average gain / loss (handles a zero average loss)
  to_rsi <- function(avg_gain, avg_loss) {
    if (avg_loss == 0) {
      if (avg_gain == 0) return(50)   # no movement at all
      return(100)                     # only gains
    }
    rs <- avg_gain / avg_loss
    100 - (100 / (1 + rs))
  }

  # Calculate the average gains and average losses for the first 'period' changes
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  rsi_values[period + 1] <- to_rsi(avg_gain, avg_loss)

  # Calculate RSI values using the Wilder's smoothing method
  if (period + 2 <= n) {
    for (i in (period + 2):n) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
      rsi_values[i] <- to_rsi(avg_gain, avg_loss)
    }
  }

  return(rsi_values)
}
