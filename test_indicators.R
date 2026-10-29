# ------------------------------------------------------------------
# test_indicators.R - checks every indicator and runs the examples
# from the assignment brief.  Run from this folder:
#     Rscript test_indicators.R
# Reference values come from base R / stats (used ONLY for testing,
# never inside the indicator implementations).
# ------------------------------------------------------------------
for (f in c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
            "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")) source(f)

passed <- 0; failed <- 0
check <- function(name, ok) {
  if (isTRUE(ok)) { passed <<- passed + 1; cat(sprintf("  PASS  %s\n", name)) }
  else            { failed <<- failed + 1; cat(sprintf("  FAIL  %s\n", name)) }
}
near <- function(a, b, tol = 1e-8) length(a) == length(b) && all(abs(a - b) < tol, na.rm = TRUE) &&
                                     identical(is.na(a), is.na(b))
throws <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

cat("\n== SMA ==\n")
s <- sma(data, 3); print(s)
check("matches stats::filter rolling mean", near(s, as.numeric(stats::filter(data, rep(1/3, 3), sides = 1))[3:9]))
check("length = n - period + 1", length(s) == 7)
check("error when data shorter than period", throws(sma(1:2, 3)))

cat("\n== EMA ==\n")
e <- ema(data, 3); print(e)
ref <- data[1]; for (i in 2:9) ref[i] <- 0.5 * data[i] + 0.5 * ref[i - 1]
check("matches hand-computed EMA (alpha = 0.5)", near(e, ref))
check("first value seeded with data[1]", e[1] == data[1])

cat("\n== MACD ==\n")
m <- macd(c(100, 105, 110, 115, 120, 125, 130), 3, 5, 2); print(m)
check("macd_line = ema(3) - ema(5)", near(m$macd_line, ema(c(100,105,110,115,120,125,130),3) - ema(c(100,105,110,115,120,125,130),5)))
check("histogram = macd_line - signal_line", near(m$histogram, m$macd_line - m$signal_line))
check("returns named list of 3", identical(names(m), c("macd_line", "signal_line", "histogram")))

cat("\n== STDEV ==\n")
sd_val <- stdev(data); print(sd_val)
check("matches population sd: sd(x)*sqrt((n-1)/n)", near(sd_val, sd(data) * sqrt(8/9)))
check("constant series gives 0", stdev(rep(5, 4)) == 0)

cat("\n== LINREG ==\n")
lr <- linreg(data, 5, 0); print(lr)
win <- data[max(1, 9 - 5 + 0):min(9, 9 - 0)]
fit <- lm(win ~ seq_along(win))
check("slope matches lm()", near(lr$slope, unname(coef(fit)[2])))
check("intercept matches lm()", near(lr$intercept, unname(coef(fit)[1])))
check("perfect line recovered", near(linreg(2 * (1:10) + 3, 10, 0)$slope, 2))
check("error when length > n", throws(linreg(1:5, 6, 0)))
check("error when offset >= length", throws(linreg(1:10, 5, 5)))

cat("\n== RSI ==\n")
rdata <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
r <- rsi(rdata, 5); print(round(r, 4))
d <- diff(rdata); g <- pmax(d, 0); l <- pmax(-d, 0)
ag <- mean(g[1:5]); al <- mean(l[1:5]); ref <- rep(NA_real_, 10); ref[6] <- 100 - 100/(1 + ag/al)
for (i in 7:10) { ag <- (ag*4 + g[i-1])/5; al <- (al*4 + l[i-1])/5; ref[i] <- 100 - 100/(1 + ag/al) }
check("matches independent Wilder RSI", near(r, ref))
check("all values within 0..100", all(r >= 0 & r <= 100, na.rm = TRUE))
check("only gains -> 100", tail(rsi(1:20, 5), 1) == 100)

cat("\n== STOCH RSI ==\n")
set.seed(42); prices <- cumsum(rnorm(80)) + 100
sr <- stoch_rsi(prices, 14, 3, 3); print(lapply(sr, function(v) round(head(v, 6), 4)))
check("%K within 0..1", all(sr$k_line >= 0 & sr$k_line <= 1))
check("%D is SMA of %K", near(sr$d_line, sma(sr$k_line, 3)))
cat("  Brief's example (period 14 on only 10 prices) -> not enough data, handled gracefully:\n")
print(suppressWarnings(stoch_rsi(rdata, 14, 3, 3)))
check("short data does not crash", !throws(suppressWarnings(stoch_rsi(rdata, 14, 3, 3))))

cat("\n== CROSSOVER / CROSSUNDER ==\n")
a1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
a2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
co <- crossover(a1, a2); cu <- crossunder(a1, a2)
print(co); print(crossover(a1, a2, labels = TRUE)); print(cu)
check("crossover at index 4 only", identical(which(co), 4L))
check("crossunder none in brief example", !any(cu))
check("crossunder detected when reversed", identical(which(crossunder(a2, a1)), 4L))
check("length mismatch errors", throws(crossover(1:3, 1:4)) && throws(crossunder(1:3, 1:4)))

cat(sprintf("\n%d passed, %d failed\n", passed, failed))
if (failed > 0) quit(status = 1)
