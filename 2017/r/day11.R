# --- Day 11: Hex Ed ---
input <- scan('2017/data/input11.txt', what = '', sep = ',')

# Part 1
get_distance <- function(input) {
  stats <- setNames(rep(0, 6), c('nw', 'n', 'ne', 'se', 's', 'sw'))
  stats[names(table(input))] <- table(input)
  
  for (i in 0:2) {
    stats[i + c(1, 4)] <- stats[i + c(1, 4)] - min(stats[i + c(1, 4)])
  }
  
  for (i in 1:6) {
    pos <- (((i - 2):i) %% 6) + 1
    a <- min(stats[pos[-2]])
    stats[pos[-2]] <- stats[pos[-2]] - a
    stats[pos[2]] <- stats[pos[2]] + a
  }
  
  sum(stats)
}

get_distance(input)

# Part 2
sapply(seq(input), \(i) get_distance(input[1:i])) |>
  max()
