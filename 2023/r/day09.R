# --- Day 9: Mirage Maintenance ---
input <- read.delim("2023/data/input09.txt", sep = ' ', header = FALSE)

# Part 1
res <- c()

for (i in seq(nrow(input))) {
  a <- input[i,] |>
    unlist() |>
    list()
  
  while (!all(tail(a, 1)[[1]] == 0)) {
    a <- tail(a, 1)[[1]] |>
      diff() |>
      list() |>
      append(a, values = _)
  }
  
  res <- cumsum(sapply(a, tail, n = 1)) |>
    tail(1) |>
    append(res)
  
}
sum(res)