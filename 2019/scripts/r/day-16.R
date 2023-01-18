# --- Day 16: Flawed Frequency Transmission ---

input <- strsplit(scan('2019/input/input-16.txt', '') , '')[[1]] |>
  as.numeric()

# Part 1
digit_transformation <- function(input, n_phases) {
  
  base_pattern <- c(0, 1, 0, -1)
  state = input
  state_length = length(state)
  for (i in 1:n_phases) {
    
    state <- sapply(seq(state_length), \(i) {
      pattern <- rep(base_pattern, each = i, length.out = state_length + 1)[-1]
      abs(sum(pattern * state)) %% 10
    })
    
    #print(state)
  }
  return((paste(state[1:8], collapse = '')))
}

digit_transformation(input, n_phases = 100)



