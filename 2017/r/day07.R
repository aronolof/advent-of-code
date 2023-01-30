# --- Day 7: Recursive Circus ---
input <- readLines('2017/data/test07.txt') |>
  strsplit('[^a-z0-9]+')

# Part 1
x <- unlist(sapply(input, \(x) x[1]))
y <- unlist(sapply(input, \(x) x[-1:-2]))
x[!(x %in% y)]

# Part 2

