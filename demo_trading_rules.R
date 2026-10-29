# ------------------------------------------------------------------
# demo_trading_rules.R - builds one OHLCV data frame per symbol,
# adds the indicators, filters stocks, and applies buy/sell rules.
# Usage:  Rscript demo_trading_rules.R [path/to/your_dataset.csv]
# The CSV needs columns: symbol, date, open, high, low, close, volume
# (your Assignment 1 dataset). Without a path, sample data is generated.
# ------------------------------------------------------------------
for (f in c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
            "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")) source(f)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  raw <- read.csv(args[1], stringsAsFactors = FALSE)
  names(raw) <- tolower(names(raw))
} else {
  set.seed(7)
  make <- function(sym, drift) {
    close <- 100 * cumprod(1 + rnorm(120, drift, 0.015))
    data.frame(symbol = sym, date = seq(as.Date("2026-01-02"), by = "day", length.out = 120),
               open = close * (1 + rnorm(120, 0, 0.003)), high = close * 1.01,
               low = close * 0.99, close = close, volume = round(runif(120, 1e5, 5e5)))
  }
  raw <- rbind(make("AAA", 0.002), make("BBB", -0.001), make("CCC", 0.0005))
}
raw$date <- as.Date(raw$date)

# 1. One data frame per symbol
frames <- split(raw[, c("date", "open", "high", "low", "close", "volume")], raw$symbol)

# 2. Filter by time period (last 90 days of each symbol's data)
frames <- lapply(frames, function(df) {
  df <- df[order(df$date), ]
  df[df$date > max(df$date) - 90, ]
})

# 3. Add indicators to each data frame
frames <- lapply(frames, function(df) {
  n <- nrow(df)
  df$ema_fast <- ema(df$close, 12)
  df$ema_slow <- ema(df$close, 26)
  df$sma_20   <- c(rep(NA, 19), sma(df$close, 20))
  m <- macd(df$close, 12, 26, 9)
  df$macd <- m$macd_line; df$macd_signal <- m$signal_line
  df$rsi_14 <- rsi(df$close, 14)
  # 4. Trading rules: BUY on fast-EMA crossover with RSI < 70,
  #    SELL on fast-EMA crossunder or RSI > 70
  up <- crossover(df$ema_fast, df$ema_slow); dn <- crossunder(df$ema_fast, df$ema_slow)
  df$signal <- ifelse(up & !is.na(df$rsi_14) & df$rsi_14 < 70, "BUY",
               ifelse(dn | (!is.na(df$rsi_14) & df$rsi_14 > 70), "SELL", ""))
  df
})

# 5. Filter stocks on indicator criteria: uptrend (positive 20-day regression
#    slope) and MACD above its signal line on the latest bar
summary_tbl <- do.call(rbind, lapply(names(frames), function(s) {
  df <- frames[[s]]; last <- nrow(df)
  data.frame(symbol = s, last_close = round(df$close[last], 2),
             slope_20 = round(linreg(df$close, 20, 0)$slope, 4),
             volatility = round(stdev(df$close[(last - 19):last]), 3),
             rsi_14 = round(df$rsi_14[last], 1),
             macd_bullish = df$macd[last] > df$macd_signal[last],
             buys = sum(df$signal == "BUY"), sells = sum(df$signal == "SELL"))
}))
print(summary_tbl, row.names = FALSE)
cat("\nStocks passing the filter (uptrend + bullish MACD):\n")
print(subset(summary_tbl, slope_20 > 0 & macd_bullish)$symbol)
cat("\nRecent signals for", names(frames)[1], ":\n")
f1 <- frames[[1]]; print(tail(f1[f1$signal != "", c("date", "close", "rsi_14", "signal")], 5), row.names = FALSE)
