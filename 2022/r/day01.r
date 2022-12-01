# --- Day 1: Calorie Counting ---

input <- as.numeric(readLines("2022/data/input01.txt"))

# Part 1
max(sapply(split(input, cumsum(is.na(input))), sum, na.rm = T))

# Part 2
sum(sort(sapply(split(input, cumsum(is.na(input))), sum, na.rm = T), T)[1:3])
