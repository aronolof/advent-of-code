# --- Day 5: A Maze of Twisty Trampolines, All Alike ---

input <- scan("2017/data/input05.txt")

# Part 1
maze <- function(x) {
  i <- 1
  n_steps <- 0
  while(TRUE) {
    n_steps <- n_steps + 1
    x[i] <- x[i] + 1
    j <- i
    i <- i + x[i] - 1
    if(i > length(x)) return(n_steps)
  }
}
maze(input)
