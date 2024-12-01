input <- readLines("2023/data/input14.txt") |>
  strsplit('') |>
  do.call(rbind, args = _)

# Part 1
apply(input, 2, \(x) {
  split(x, cumsum((x == '#') | (c('#', head(x, -1)) == '#'))) |>
    sapply(sort, decreasing = TRUE) |>
    unlist()
  }) |>
  (\(x) rowSums(x == 'O') * rev(seq(nrow(x))))() |>
  sum()

# Part 2
tilt <- function(platform, m, d) {
  apply(platform, m, \(a) {
    split(a, cumsum((a == '#') | (c('#', head(a, -1)) == '#'))) |>
      sapply(sort, decreasing = d) |>
      unlist() 
  }) 
}

# Part 2
platform <- input
load_history <- c()
for(i in 1:150) {
  platform <- tilt(platform, 2, TRUE) # North
  platform <- tilt(platform, 1, TRUE) |> t() # West
  platform <- tilt(platform, 2, FALSE) # South
  platform <- tilt(platform, 1, FALSE) |> t() # East
  load_history <- (rowSums(platform == 'O') * rev(seq(nrow(platform)))) |>
    sum() |>
    append(load_history, values = _)
  print(i)
}

# plot(tail(load_history, 100), type ='l')

cycle_length <- which(load_history == tail(load_history, 1)) |>
  diff() |>
  tail(1)

which(load_history == load_history[150]) |> diff()

load_history[seq(1000000000 %% cycle_length, length(load_history), cycle_length)] |>
  tail(1)

