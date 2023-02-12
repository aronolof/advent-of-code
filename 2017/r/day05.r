# --- Day 5: A Maze of Twisty Trampolines, All Alike ---
input <- scan("2017/data/input05.txt")

# Part 1
maze <- function(x, step2 = FALSE) {
  i <- 1
  n_steps <- 0
  while (TRUE) {
    n_steps <- n_steps + 1
    j <- i
    i <- i + x[i]
    
    if (step2 && x[j] >= 3) {
      x[j] <- x[j] - 1
    } else {
      x[j] <- x[j] + 1
    }
    
    if (i > length(x)) {
      return(n_steps)
    }
  }
}
maze(input)

# Part 2
maze(input, step2 = TRUE)
