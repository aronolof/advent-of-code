# --- Day 3: Rucksack Reorganization ---
items <- readLines("input.txt") |>
  lapply(\(x) (utf8ToInt(x) - 38) %% 58)

# Part 1
items |>
  sapply(\(x) Reduce(intersect, split(x, seq(x) <= length(x)/2))) |> 
  sum()

# Part 2
split(items, (seq(length(items)) - 1) %/% 3) |>
  sapply(\(x) Reduce(intersect, x)) |>
  sum()
