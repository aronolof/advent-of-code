# --- Day 10: Elves Look, Elves Say ---

input <- strsplit('1113222113', '')[[1]] |>
  as.numeric()

look_and_say <- function(input, times = 1) {
  x <- input
  for (i in seq(times)) {
    l <- rle(x)$lengths
    v <- rle(x)$values
    x <- c(rbind(l, v))
  }
  return(length(x))
}

# Part 1
look_and_say(input, times = 40) 

# Part 2
look_and_say(input, times = 50)
