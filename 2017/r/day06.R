# --- Day 6: Memory Reallocation ---

input <- scan('2017/data/input06.txt')

# Part 1
redistribute_blocks <- function(input) {
  banks <- input
  past_states <- paste(banks, collapse = ' ')
  i <- 1
  
  while (TRUE) {
    i <- i + 1
    is_max <- which.max(banks)
    input[is_max]
    
    blocks <- table((((is_max + seq(banks[is_max])) - 1) %% length(banks)) + 1)
    banks[is_max] <- 0
    banks[as.numeric(names(blocks))] <- banks[as.numeric(names(blocks))] + blocks
    
    past_states[i] <- paste(banks, collapse = ' ')
    if (duplicated(past_states)[i]) return(list(cycle = i - 1, state = banks))
  }
}

answer_1 <- redistribute_blocks(input)
answer_1$cycle

# Part 2
redistribute_blocks(answer_1$state)$cycle
