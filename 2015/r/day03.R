# --- Day 3: Perfectly Spherical Houses in a Vacuum ---
input <- scan('2015/data/input03.txt', what = '') |>
  strsplit('') |>
  unlist()

dirs <- c('<' = -1, '>' = 1, '^' = 1i, 'v' = -1i)

# Part 1
c(0, dirs[input]) |>
  cumsum() |>
  unique() |>
  length()

# Part 2
sapply(c(TRUE, FALSE), \(b) {
  c(0, dirs[input][c(b, !b)]) |>
    cumsum()
}) |>
  as.vector() |>
  unique() |>
  length()
