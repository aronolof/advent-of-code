# --- Day 11: Cosmic Expansion ---
input <- readLines("2023/data/input11.txt")

# Part 1
space <- sapply(strsplit(input, ''), \(x) x) |>
  apply(1, \(x) {
    if (all(x == '.')) return(unname(rbind(x, x)))
    x
  }) |>
  do.call(rbind, args = _) |>
  apply(2, \(x) {
    if (all(x == '.')) return(unname(rbind(x, x)))
    x
  }) |>
  do.call(rbind, args = _)

b <- which(space == '#', arr.ind = TRUE)

r <- apply(b, 1, \(x) abs(b[,1] - x[1]) + abs(b[,2] - x[2])) 
sum(r[row(r) > col(r)])

# Part 2
space <- do.call(rbind, strsplit(input, '')) |>
  (\(x) x == '.')() |>
  apply(1, \(x) x + rep(all(x), length(x))) |>
  apply(1, \(x) x + rep(all(x), length(x)))

b1 <- which(space == 0, arr.ind = TRUE)

mult <- (1000000-1)

merge(b1, b1, by = NULL) |>
  apply(1, \(x) {
    x1 <- space[x[1]:x[3], x[2]:x[4], drop = FALSE]
    sum(1 + mult * (x1[row(x1) == 1 | col(x1) == 1] >= 2)) - 1
}) |>
  sum() |>
  prod(0.5)

