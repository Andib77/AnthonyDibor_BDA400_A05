# ------------------------------------------------------------------
# Moving Average Convergence Divergence (MACD)
# BDA400 - Assignment 5 | Base R only (no external libraries)
# ------------------------------------------------------------------
# MACD line   = EMA(data, short_period) - EMA(data, long_period)
# Signal line = EMA(MACD line, signal_period)
# Histogram   = MACD line - Signal line
# returns: list(macd_line, signal_line, histogram), each same length as data
# ------------------------------------------------------------------
if (!exists("ema", mode = "function")) source("ema.R")

macd <- function(data, short_period, long_period, signal_period) {
  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema  <- ema(data, long_period)

  # Calculate the MACD line
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  result <- list(
    macd_line   = macd_line,
    signal_line = signal_line,
    histogram   = histogram
  )
  return(result)
}
