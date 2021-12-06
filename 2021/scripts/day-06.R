# --- Day 6: Lanternfish ---

input <- scan('2021/input/input-06.txt', sep = ',')

# Part 1
simulation <- function(days) {
  state <- table(factor(input, levels = 0:9))
  
  for(i in seq(days)) {
    state[c(8, 10)] <- state[1] + c(state[8], 0)
    state[1:9] <- state[2:10]
  }
  state[1:9]
}

simulation(80) |> sum()

# Part 2
# simulation(256) |> sum() # Does not work
# bit64::as.integer64(as.vector(simulation(256))) |> sum() # The easy solution

# The 32 bit way
digit_sum <- apply(matrix(as.numeric(unlist(strsplit(as.character(simulation(256)), ''))), ncol = 9), 1, sum)

for(j in rev(seq(digit_sum)[-1])) {
  digit_sum[c(j-1, j)] <- c(digit_sum[j-1] + digit_sum[j]%/%10, digit_sum[j]%%10)
}

paste(digit_sum, collapse='')
