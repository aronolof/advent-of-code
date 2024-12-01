# --- Day 10: Pipe Maze ---
input <- readLines("2023/data/test10.txt")

# Part 1
m <- input |>
  sapply(\(x) strsplit(x, '')[[1]]) |>
  unname() |>
  t()

pipe_maze <- function (start_direction) {
  # Setup
  dist_m <- (m == 0) + Inf
  start <- which(m == 'S', arr.ind = T)
  dist_m[start] <- 0
  
  direction <- start_direction
  last_pos <- start
  
  repeat {
    pos <- last_pos + c(Re(direction), Im(direction))
    
    tile <- m[pos]
    if (length(tile) == 0) return(NA)
    if (tile == 'S') {
      return(list(
        median(dist_m[dist_m > 0 & dist_m < Inf]),
        dist_m
      ))
    }
    
    if (direction == (0 + 1i)) {
      direction <- c('-' = 0 + 1i, '7' = 1 + 0i, 'J' = -1 + 0i)[tile]
    } else if (direction == (0 - 1i)) {
      direction <- c('-' = 0 - 1i, 'F' = 1 + 0i, 'L' = -1 + 0i)[tile]
    } else if (direction == (1 + 0i)) {
      direction <- c('|' = 1 + 0i, 'L' = 0 + 1i, 'J' = 0 - 1i)[tile]
    } else if (direction == (-1 + 0i)) {
      direction <- c('|' = -1 + 0i, '7' = 0 - 1i, 'F' = 0 + 1i)[tile]
    }
    if (is.na(direction)) return(NA)
    
    dist_m[pos] <- dist_m[last_pos] + 1
    last_pos <- pos
  }
}

for (d in c(0 + 1i, 0 - 1i, 1 + 0i, -1 + 0i)) {
  result <- pipe_maze(1 + 0i)
  if (!is.na(result[[1]])) break
}

result[[1]]

result[[2]] != Inf
sum(result[[2]] != Inf)
sum(result[[2]] == Inf)

result[[2]]



