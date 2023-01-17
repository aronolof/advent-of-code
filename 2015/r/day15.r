# --- Day 15: Science for Hungry People ---
input <- readLines('2015/data/input15.txt') |>
  strsplit('[^0-9-]+') |>
  sapply(\(x) as.numeric(x[2:6])) |>
  t()

# Part 1
find_top_score <- function(input) {
  state <- rep(100/nrow(input), nrow(input))
  top_score <- 0
  
  check_score <- function(state) {
    (input[,-5] * state) |>
      colSums() |>
      pmax(0) |>
      prod()
  }
  
  change <- expand.grid(seq(state), seq(state))
  change <- change[change$Var1 != change$Var2,]
  
  
  while (TRUE) {
    adjacent_scores <- apply(change, 1, \(x) {
      new_state <- state
      new_state[x[1]] <- new_state[x[1]] - 1
      new_state[x[2]] <- new_state[x[2]] + 1
      check_score(new_state)
    })
    
    if (any(adjacent_scores >= check_score(state))) {
      move <- change[which.max(adjacent_scores),] |>
        unlist()
      state[move[1]] <- state[move[1]] - 1
      state[move[2]] <- state[move[2]] + 1
    } else {
      return(check_score(state))
    }
    top_score <- max(top_score, check_score(state))
  }
}
  
find_top_score(input)
  
