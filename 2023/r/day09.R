# --- Day 9: Mirage Maintenance ---
input <- read.delim("2023/data/input09.txt", sep = ' ', header = FALSE)

# Part 1
apply(input, 1, \(x) {
  h <- list(x)

  while (!all(tail(h, 1)[[1]] == 0)) {
    h <- tail(h, 1)[[1]] |>
      diff() |>
      list() |>
      append(h, values = _)
  }
  
  sapply(h, tail, n = 1) |>
    cumsum() |>
    tail(1)
}) |>
  sum()

# Part 2
apply(input, 1, \(x) {
  h <- list(x)
  
  while (!all(tail(h, 1)[[1]] == 0)) {
    h <- tail(h, 1)[[1]] |>
      diff() |>
      list() |>
      append(h, values = _)
  }
  
  first_values <- rev(sapply(h, head, n = 1))[-1]
  acc <- 0
  for (x in first_values) acc <- x - acc
  acc
}) |>
  sum()
