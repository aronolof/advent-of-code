# --- Day 3: Squares With Three Sides ---
input <- read.table("2016/data/input03.txt") 

possible_triangle <- function(x) {
  x[[1]] + x[[2]] > x[[3]] &
    x[[2]] + x[[3]] > x[[1]] &
    x[[3]] + x[[1]] > x[[2]]
}

# Part 1
input |>
  possible_triangle() |>
  sum()

# Part 2
input |>
  unlist() |>
  matrix(ncol = 3, byrow = TRUE) |>
  data.frame() |>
  possible_triangle() |>
  sum()
