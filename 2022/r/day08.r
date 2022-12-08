# --- Day 8: Treetop Tree House ---
m <- readLines("2022/data/input08.txt") |>
  sapply(\(x) as.integer(strsplit(x, '')[[1]])) %>%
  unname()

visibility = m^0
score = m * 0

for (i in 2:(nrow(m) - 1)) {
  for (j in 2:(ncol(m) - 1)) {
    
    surrounding_trees <- list(
      m[i, 1:(j - 1)],
      rev(m[i, (j + 1):ncol(m)]),
      m[1:(i - 1), j],
      rev(m[(i + 1):nrow(m), j])
    )
    
    visibility[i,j] <- surrounding_trees |>
      sapply(\(x) max(x) < m[i,j]) |>
      any()
    
    score[i,j]  <-
      surrounding_trees |>
      lapply(\(x) cumsum(m[i,j] <= x)) |>
      sapply(\(x) sum(x == max(x))) |>
      prod()
    
  }
}

# Part 1
sum(visibility)
# Part 2
max(score)
