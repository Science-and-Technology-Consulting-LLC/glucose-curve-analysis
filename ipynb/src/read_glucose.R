# Read the Lingo export. Out of range readings are set to 54, the highest value they can have.
read_glucose <- function(path) {
  glucose <- read.csv(path, stringsAsFactors = FALSE)
  glucose$out_of_range <- glucose$glucose_mg_dl == "Out of range"
  glucose$glucose <- suppressWarnings(as.numeric(glucose$glucose_mg_dl))
  glucose$glucose[glucose$out_of_range] <- 54
  glucose$time <- as.POSIXct(substr(glucose$time_local_with_offset, 1, 16),
                             format = "%Y-%m-%dT%H:%M", tz = "UTC")
  glucose
}
