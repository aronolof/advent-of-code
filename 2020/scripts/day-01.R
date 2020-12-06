# --- Day 1: Report Repair ---

input <- scan("2020/input/input-01.txt")

# Part 1
prod(input[(2020-input) %in% input])

# Part 2
(function(x) prod(x[rowSums(x) == 2020,][1,]))(expand.grid(input, input, input))
