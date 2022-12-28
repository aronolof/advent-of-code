# --- Day 1: Not Quite Lisp ---
input <- scan('2015/data/input01.txt', what = '') |>
  strsplit('') |>
  unlist()

# Part 1
c('(' = 1, ')' = -1)[input] |>
  sum()

# Part 2
(cumsum(c('(' = 1, ')' = -1)[input]) == -1) |>
  which() |>
  head(1)
