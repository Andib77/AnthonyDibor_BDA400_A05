# ------------------------------------------------------------------
# Crossover
# BDA400 - Assignment 5 | Base R only (no external libraries)
# ------------------------------------------------------------------
# TRUE at i when arr1[i] > arr2[i] and arr1[i-1] <= arr2[i-1]
# (arr1 has just crossed ABOVE arr2). First element is always FALSE.
# returns: logical vector, same length as the inputs.
# Optional: labels = TRUE returns the pseudocode's "Up"/"Down"/"None".
# ------------------------------------------------------------------
crossover <- function(arr1, arr2, labels = FALSE) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  n <- length(arr1)

  # Initialize a vector to store the crossover signals
  crossover_signals <- rep(FALSE, n)
  direction         <- rep("None", n)

  # Check for crossovers at each data point
  if (n >= 2) {
    for (i in 2:n) {
      if (anyNA(c(arr1[i], arr2[i], arr1[i - 1], arr2[i - 1]))) next
      if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- TRUE
        direction[i] <- "Up"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        direction[i] <- "Down"
      }
    }
  }

  if (labels) return(direction)
  return(crossover_signals)
}
