# --- Day 20: Grove Positioning System ---
input <- scan('2022/data/input20.txt') 

grove_position <- function(input, decryption_key = 1, loops = 1) {
  decrypted_input <- input * decryption_key
  index <- seq(decrypted_input)
  max_index <- max(index)
  
  for (j in seq(loops)) {
    
    for (i in seq(decrypted_input)) {
      current_loc <- which(index == i)
      new_loc <- ((current_loc + decrypted_input[i] - 1) %% max_index) + 1
      
      if (abs(decrypted_input[i]) >= max_index) {
        new_loc <- new_loc + sign(decrypted_input[i])
      } 
      
      index_new <- seq(index)
      index_new[current_loc] <- new_loc + sign(decrypted_input[i])/2
      
      # Not necessary, just helps align wrap-around with example data
      if (index_new[current_loc] == 0.5) {
        index_new[current_loc] <- max_index + 0.5
      } else if (index_new[current_loc] == max_index + 0.5) {
        index_new[current_loc] <- 0.5
      }
      
      index <- index[order(index_new)]
    }
  }
  
  zero_location <- which(decrypted_input[index] == 0)
  grove_locations <- ((zero_location + c(1000, 2000, 3000) - 1) %% max_index) + 1
  (decrypted_input[index][grove_locations])
  #decrypted_input[index]
}

# Part 1
grove_position(input) |>
  sum()

# Part 2
# Does not properly yet
grove_position(input, 811589153, 10) |>
  sum() |>
  paste()
