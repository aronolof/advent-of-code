# --- Day 14: Reindeer Olympics ---

reindeer_attributes <- readLines('2015/data/input14.txt') |>
  strsplit(split = '[^0-9]+') |>
  lapply(\(x_str) {
    x <- as.numeric(x_str)
    c(speed = x[2], duration = x[3], rest = x[4])
  })

# Part 1
part_1 <- function(reindeer_attributes, seconds) {
  sapply(reindeer_attributes, function(x) {
    full_cycle_seconds <- sum(x[2:3])
    ((((seq(seconds) - 1) %% full_cycle_seconds) < x[2]) * x[1]) |>
      cumsum() |>
      tail(1)
  }) |>
    max()
}

part_1(reindeer_attributes, seconds = 2503)

# Part 2
part_2 <- function(reindeer_attributes, seconds) {
  sapply(reindeer_attributes, function(x) {
    full_cycle_seconds <- sum(x[2:3])
    ((((seq(seconds) - 1) %% full_cycle_seconds) < x[2]) * x[1]) |>
      cumsum() 
  }) |>
    apply(1, \(x) x == max(x)) |>
    apply(1, sum) |>
    max()
}
part_2(reindeer_attributes, seconds = 2503)
