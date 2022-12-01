# --- Day 2: Corruption Checksum ---

input <- read.table('2017/data/input02.txt')

# Part 1
sum(apply(input, 1, \(x) diff(range(x))))

# Part 2
sum(apply(input, 1, \(x) {
  divisible <- sapply(x, \(y) x%%y == 0)
  values <- x[which(divisible[rowSums(divisible) == 2,])]
  max(values)/min(values)
}))

