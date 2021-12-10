# --- Day 9: Smoke Basin ---

input <- scan('2021/input/input-09.txt', what = character()) |>
  (\(x) matrix(as.numeric(unlist(strsplit(x, ''))), nchar(x)[1]))()

# Part 1
low_points <- input < cbind(input[,-1], 10) &
  input < cbind(10, input[,-ncol(input)]) &
  input < rbind(input[-1,], 10) &
  input < rbind(10, input[-nrow(input),])

sum(input[low_points] + 1)

# Part 2
basins <- matrix(seq(input), nrow(input))
basins[input == 9] <- 0

while(TRUE) {
  previous_basins <- basins
  
  basins <- pmax(basins, cbind(basins[,-1], NA), na.rm = TRUE) * (input != 9)
  basins <- pmax(basins, cbind(NA, basins[,-ncol(basins)]), na.rm = TRUE) * (input != 9)
  basins <- pmax(basins, rbind(basins[-1,], NA), na.rm = TRUE) * (input != 9)
  basins <- pmax(basins, rbind(NA, basins[-nrow(basins),]), na.rm = TRUE) * (input != 9)
  
  if(all(basins == previous_basins)) break
}

prod(tail(sort(table(basins[basins != 0])), 3))
