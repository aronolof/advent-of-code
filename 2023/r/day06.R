# --- Day 6: Wait For It ---

input <- readLines("2023/data/input06.txt")

# Part 1
do.call(rbind, strsplit(input, ' +'))[,-1] |>
  apply(1, as.numeric) |>
  apply(1, \(x) sum(seq(x[1]) * (x[1] - seq(x[1])) > x[2])) |>
  prod()

# Part 2

find_edge <- function(edge) {
  next_range <- c(1, time)
  
  repeat {
    ran <- seq(next_range[1], next_range[2], length.out = 100) |>
      as.integer() |>
      unique()
    
    res <- sapply(ran, \(w) (w * (time - w)) > distance)
    
    if (edge == 'left') next_range <- ran[head(which(res), 1) + c(-1, 0)]
    if (edge == 'right') next_range <- ran[tail(which(res), 1) + c(0, 1)]
    
    if (diff(next_range) == 1) return(next_range[1])
  }
}
find_edge('right') - find_edge('left') 