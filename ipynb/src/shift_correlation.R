# Correlation between two 48-value daily profiles (one value per half hour),
# after moving the second profile earlier by each number of hours.
shift_correlation <- function(fixed, moving, hours = -3:3) {
  slots <- length(moving)
  sapply(hours, function(h) {
    shifted <- moving[(seq_len(slots) - 1 + 2 * h) %% slots + 1]
    cor(fixed, shifted)
  })
}
