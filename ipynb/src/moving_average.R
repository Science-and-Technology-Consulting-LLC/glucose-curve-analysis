# Average of the last k days, using only the days marked ok. Needs at least min_days of them.
moving_average <- function(values, ok, k = 7, min_days = 4) {
  sapply(seq_along(values), function(i) {
    window <- max(1, i - k + 1):i
    window <- window[ok[window]]
    if (length(window) >= min_days) mean(values[window]) else NA
  })
}
