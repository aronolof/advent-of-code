# --- Day 12: JSAbacusFramework.io ---

input <- readLines('2015/data/input12.txt')

# Part 1
gsub('[^0-9-]+', ',', input) |>
  strsplit(',') |>
  unlist() |>
  as.numeric() |>
  sum(na.rm = TRUE)
