# --- Day 5: Doesn't He Have Intern-Elves For This? ---
input <- readLines('2015/data/input05.txt')

# Part 1
input |>
  strsplit('') |>
  sapply(\(x) {
    all(
      sum(x %in% c('a', 'e', 'i', 'o', 'u')) >= 3,
      any(head(x, -1) == tail(x, -1)),
      sum(c('ab', 'cd', 'pq', 'xy') %in% paste0(head(x, -1), tail(x, -1))) == 0
    )
  }) |>
  sum()

# Part 2
input |>
  strsplit('') |>
  sapply(\(x) {
    pairs <- paste0(head(x, -1), tail(x, -1))
    all(
      any(sapply(pairs, \(y) diff(range(which(y == pairs))) >= 2)),
      any(head(x, -2) == tail(x, -2))
    )
  }) |>
  sum()
    