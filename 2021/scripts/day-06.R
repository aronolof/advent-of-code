# --- Day 6: Lanternfish ---

options(scipen = 999)

input <- scan('2021/input/input-06.txt', sep = ',')

# Part 1
simulation <- function(days) {
  state <- as.double(table(factor(input, levels = 0:8)))
  
  for(i in seq(days)) {
    state <- c(state[2:7], state[8] + state[1], state[9], state[1])
  }
  sum(state)
}

simulation(80)

# Part 2
simulation(256) 
