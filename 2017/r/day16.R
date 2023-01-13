# --- Day 16: Permutation Promenade ---
input <- scan('2017/data/input16.txt', what = '', sep = ',')

# Part 1
permutation_promenade <- function(input, start_positions = letters[1:16]) {
  programs <- start_positions
  
  for (instruction in input) {
    dance_move <- substr(instruction, 1, 1)
    steps <- strsplit(substring(instruction, 2), '/')[[1]]
  
    if (dance_move == 's') {
      programs <- programs[(((seq(programs) - 1) - as.numeric(steps[1])) %% 16) + 1]
      
    } else if (dance_move == 'x') {
      exchange <- programs[as.numeric(steps) + 1]
      programs[as.numeric(steps[1]) + 1] <- exchange[2]
      programs[as.numeric(steps[2]) + 1] <- exchange[1]
      
    } else if (dance_move == 'p') {
      partner_pos <- which(programs %in% steps)
      partner <- programs[partner_pos]
      programs[partner_pos[1]] <- partner[2]
      programs[partner_pos[2]] <- partner[1]
    } 
  }
  return(programs)
}

permutation_promenade(input) |>
  paste(collapse = '')

# Part 2
current_position <- letters[1:16]
dance_history <- list(current_position)

while (TRUE) {
  current_position <- permutation_promenade(input, start_positions = current_position)
  if (all(current_position == dance_history[[1]])) {
    break
  } else {
    dance_history <- append(dance_history, list(current_position)) 
  }
}

dance_history[[((1000000000) %% length(dance_history)) + 1]]  |>
  paste(collapse = '')
