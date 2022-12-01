# --- Day 1: Calorie Counting ---

input <- as.numeric(readLines("2022/data/input.txt"))

# Part 1
sort(sapply(split(input, cumsum(is.na(input))), sum, na.rm=T), TRUE)[1]

# Part 2
sum(sort(sapply(split(input, cumsum(is.na(input))), sum, na.rm=T), TRUE)[1:3])
