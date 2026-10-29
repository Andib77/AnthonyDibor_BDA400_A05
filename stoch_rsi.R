# ------------------------------------------------------------------
# Stochastic RSI (StochRSI)
# BDA400 - Assignment 5 | Base R only (no external libraries)
# ------------------------------------------------------------------
# k_values = (RSI - min(RSI)) / (max(RSI) - min(RSI))   -> range 0..1
# %K line  = SMA(k_values, k_period)
# %D line  = SMA(%K line, d_period)
# returns: list(k_line, d_line)
# ------------------------------------------------------------------
if (!exists("rsi", mode = "function")) source("rsi.R")
if (!exists("sma", mode = "function")) source("sma.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)
  rsi_valid  <- rsi_values[!is.na(rsi_values)]   # drop the NA warm-up values

  if (length(rsi_valid) < k_period + d_period - 1) {
    warning("Not enough RSI values for the requested %K / %D periods; returning empty lines")
    return(list(k_line = numeric(0), d_line = numeric(0)))
  }

  # Calculate the StochRSI (normalise RSI to the 0..1 range)
  min_rsi <- min(rsi_valid)
  max_rsi <- max(rsi_valid)
  if (max_rsi == min_rsi) {
    k_values <- rep(0, length(rsi_valid))      # flat RSI: avoid division by zero
  } else {
    k_values <- (rsi_valid - min_rsi) / (max_rsi - min_rsi)
  }

  # Calculate the %K line (StochRSI)
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
