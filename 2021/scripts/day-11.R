# --- Day 11: Dumbo Octopus ---

input <- apply(Reduce(rbind, strsplit(readLines('2021/input/input-11.txt'), '')), 2, as.numeric)

state <- input
flashes <- 0
prop_flashing <- c()

for (i in 1:300) {
  state <- (state + 1)%%10
  
  while(sum(state == 0) != 0) {
    padding <- cbind(10, rbind(10, state, 10), 10)
    
    neighbors <- mapply(
      \(i, j) padding[(2:11)+i, (2:11)+j] == 0,
      i = rep(-1:1, each = 3)[-5],
      j = rep(-1:1, 3)[-5],
      SIMPLIFY = FALSE
    ) |>
      (\(x) Reduce(`+`, x))()
    state[state == 0] <- state[state == 0] - 1
    
    state[state > 0] <- (state[state > 0] + neighbors[state > 0])
    state[state > 9] <- 0
  }
  
  flashes <- flashes + sum(state < 0)
  prop_flashing[i] <- mean(state < 0)
  state[state < 0] <- 0
  

  
}
state
flashes
which(prop_flashing == 1)[1]
