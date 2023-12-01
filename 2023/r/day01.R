# --- Day 1: Trebuchet?! ---

input <- readLines("2023/data/input01.txt")

# Part 1
gsub('\\D', '', input) |>
  sapply(\(x) paste0(substr(x, 1, 1), substring(x, nchar(x)))) |>
  as.numeric() |>
  sum()

# Part 2
d <- c('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', '#', 1:9)

sapply(input, \(x) {
  matches <- sapply(d, gregexpr, x)
  
  first = sapply(matches, min)
  first[first == -1] <- 99
  
  last = sapply(matches, max)
  
  sum(10 * which.min(first) %% 10, which.max(last) %% 10)
}) |>
  sum()
