# AnthonyDibor_BDA400_A05 — Technical Analysis using R (Development Phase)

Base-R implementations (no packages) of nine technical indicators, one script per indicator,
following the templates and function names from the assignment brief.

| File | Function | Returns |
|---|---|---|
| `sma.R` | `sma(data, period)` | vector, length n − period + 1 |
| `ema.R` | `ema(data, period)` | vector, length n |
| `macd.R` | `macd(data, short_period, long_period, signal_period)` | list: `macd_line`, `signal_line`, `histogram` |
| `stdev.R` | `stdev(data)` | single number (population SD) |
| `linreg.R` | `linreg(regressionSource, regressionLength, regressionOffset)` | list: `slope`, `intercept`, `predicted_values` |
| `rsi.R` | `rsi(data, period)` | vector, length n (first `period` values NA) |
| `stoch_rsi.R` | `stoch_rsi(data, period, k_period, d_period)` | list: `k_line`, `d_line` |
| `crossover.R` | `crossover(arr1, arr2)` | logical vector, length n |
| `crossunder.R` | `crossunder(arr1, arr2)` | logical vector, length n |

## How to run
1. Install R (4.x). No packages are needed.
2. Open a terminal (or RStudio) **in this folder** — scripts that depend on others
   (`macd.R` → `ema.R`, `stoch_rsi.R` → `rsi.R`, `sma.R`) source them by relative path.
   In RStudio: *Session → Set Working Directory → To Source File Location*.
3. Run the tests: `Rscript test_indicators.R` (expect `25 passed, 0 failed`).
4. Run the demo: `Rscript demo_trading_rules.R` (sample data) or
   `Rscript demo_trading_rules.R your_data.csv` (columns: symbol, date, open, high, low, close, volume).
5. To use a single function: `source("sma.R"); sma(c(10, 12, 15, 20, 18), 3)`.

See `AnthonyDibor_BDA400_A05.docx` for the explanation of each implementation.
# AnthonyDibor_BDA400_A05
