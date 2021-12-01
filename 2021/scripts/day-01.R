# --- Day 1: Sonar Sweep ---

input <- scan("2021/input/input-01.txt")

# Part 1
sum(diff(input) > 0)

# Part 2
seq(length(input)-2) |>
  sapply(\(i) sum(input[i:(i+2)])) |>
  diff() |>
  (`>`)(0) |>
  sum()
