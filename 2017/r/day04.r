# --- Day 4: High-Entropy Passphrases ---

input <- strsplit(readLines("2017/data/input04.txt"), ' ')

# Part 1
input |>
  sapply(\(x) all(table(x) == 1)) |>
  sum()

input |>
  sapply(\(x) {
    strsplit(x, '') |>
      sapply(\(y) paste(sort(y), collapse='')) |>
      (\(y) anyDuplicated(y) == 0)()
    }) |>
  sum()
