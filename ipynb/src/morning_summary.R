# One row per morning, using the clock times of the day. Readings from 2026-11-01 on are moved
# earlier by clock_shift_hours (see notebook time_of_day). A morning needs at least 60 of its
# 72 readings between 04:00 and 10:00.
# baseline = mean from 04:00 to 05:29; peak = highest reading from 05:30 to 09:59;
# rise = peak minus baseline.
morning_summary <- function(glucose, clock_shift_hours = 1,
                            change_date = as.Date("2026-11-01"),
                            first_sunday = as.Date("2026-09-27")) {
  after <- as.Date(glucose$time) >= change_date
  clock <- glucose$time - 3600 * clock_shift_hours * after
  day <- as.Date(clock)
  hour <- as.integer(format(clock, "%H")) + as.integer(format(clock, "%M")) / 60
  rows <- lapply(split(seq_along(day), day), function(i) {
    window <- i[hour[i] >= 4 & hour[i] < 10]
    if (length(window) < 60) return(NULL)
    early <- window[hour[window] < 5.5]
    later <- window[hour[window] >= 5.5]
    top <- later[which.max(glucose$glucose[later])]
    baseline <- mean(glucose$glucose[early])
    data.frame(day = day[i[1]], readings = length(window), baseline = baseline,
               peak = glucose$glucose[top], peak_hour = hour[top],
               rise = glucose$glucose[top] - baseline)
  })
  mornings <- do.call(rbind, rows)
  mornings$week <- first_sunday + 7 * ((as.integer(mornings$day - first_sunday)) %/% 7)
  mornings
}
