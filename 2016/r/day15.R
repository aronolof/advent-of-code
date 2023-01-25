# --- Day 15: Timing is Everything ---
arrangement <- readLines('2016/data/input15.txt') |>
  strsplit('[^0-9]+') |>
  lapply(\(x) as.numeric(x[c(3, 5)]))

# Part 1
find_start_time <- function(arrangement) {
  start = 0
  while (TRUE) {
    positions <- sapply(seq(arrangement), \(i) {
      (start + i + arrangement[[i]][2]) %% arrangement[[i]][1]
    })
    
    if (all(positions == 0)) {
      return(start)
    } else {
      start <- start + 1
    }
  }
}

find_start_time(arrangement)

# Part 2
find_start_time(append(arrangement, list(c(11, 0))))
