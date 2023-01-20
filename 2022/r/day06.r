# --- Day 6: Tuning Trouble ---

input <- strsplit(readLines("2022/data/input06.txt"), '')[[1]]

find_marker <- \(input, size) {
  for (i in tail(seq(input), 1 - size)) {
    if (length(unique(input[(i - size + 1):i])) == size) return(i)
  }
}

# Part 1
find_marker(input, 4)
# Part 2
find_marker(input, 14)
