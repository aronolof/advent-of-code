# --- Day 4: Scratchcards ---

input <- readLines("2023/data/input04.txt")

# Part 1
input |>
  strsplit(' \\| ') |>
  sapply(\(x) {
    card <- strsplit(x, ' +')
    win <- sum(card[[1]] %in% card[[2]])
    (win >= 1) * (2 ^ (win - 1))
  }) |>
  sum()

# Part 2