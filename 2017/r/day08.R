# --- Day 8: I Heard You Like Registers ---
input <- readLines('2017/data/input08.txt')

run_instructions <- function(input) {
  instructions <- input |>
    strsplit(' ')
  
  register_names <- sapply(instructions, \(x) x[1])
  registers <- setNames(rep(0, length(register_names)), register_names)
  highest_value <- max(registers)
  
  for (x in instructions) {
    condition <- paste(registers[x[5]], x[6], x[7]) |>
      parse(text = _) |>
      eval()
    
    if (condition) {
      new_value <- paste(registers[x[1]], c('inc' = '+', 'dec' = '-')[x[2]], x[3]) |>
        parse(text = _) |>
        eval()
      
      highest_value <- max(highest_value, new_value)
      registers[x[1]] <- new_value
    }
  }
  c('final_max_value' = max(registers), 'max_reached_value' = highest_value)
}

# Part 1
result <- run_instructions(input)
result['final_max_value']

# Part 2
result['max_reached_value']
