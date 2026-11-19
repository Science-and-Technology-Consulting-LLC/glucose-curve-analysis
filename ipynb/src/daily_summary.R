# One row per calendar day: readings, mean, standard deviation, percent of readings below 70.
# A day is complete when it has at least 230 of the 288 possible readings.
daily_summary <- function(glucose) {
  glucose$day <- as.Date(glucose$time)
  days <- seq(min(glucose$day), max(glucose$day), by = 1)
  rows <- lapply(days, function(d) {
    x <- glucose$glucose[glucose$day == d]
    data.frame(day = d, n = length(x),
               mean = if (length(x) > 1) mean(x) else NA,
               sd = if (length(x) > 1) sd(x) else NA,
               below70 = if (length(x) > 1) 100 * mean(x < 70) else NA)
  })
  daily <- do.call(rbind, rows)
  daily$complete <- daily$n >= 230
  daily
}

# One row per week, weeks starting on a Sunday, with the change from the week before.
weekly_summary <- function(glucose, first_sunday = as.Date("2026-09-27")) {
  day <- as.Date(glucose$time)
  week <- first_sunday + 7 * ((as.integer(day - first_sunday)) %/% 7)
  rows <- lapply(split(seq_along(week), week), function(i) {
    x <- glucose$glucose[i]
    data.frame(days = length(unique(day[i])), readings = length(x),
               mean = mean(x), sd = sd(x),
               below70 = 100 * mean(x < 70), above140 = 100 * mean(x > 140))
  })
  weekly <- do.call(rbind, rows)
  weekly$change_mean <- c(NA, diff(weekly$mean))
  weekly$change_sd <- c(NA, diff(weekly$sd))
  weekly$change_below70 <- c(NA, diff(weekly$below70))
  weekly
}
