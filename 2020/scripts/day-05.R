# --- Day 5: Binary Boarding ---

input <- readLines("2020/input/input-05.txt")

# Part 1
id <- sapply(strsplit(input, ""), function(x) {
    8 * sum((x == 'B')[1:7]*(2^(6:0))) + sum((x == 'R')[8:10]*(2^(2:0)))
  })

max(id)

# Part 2
setdiff(min(id):max(id), numbers)

