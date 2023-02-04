# --- Day 12: Leonardo's Monorail ---
input <- readLines("2016/data/input12.txt") |>
  strsplit(' ')

# Part 1
registers <- setNames(rep(0, 4), letters[1:4])

execute_assembunny <- function(input, registers) {
  i <- 1
  while (TRUE) {
    last_i = i
    
    if (i > length(input)) break
    instr <- input[[i]]
    if (instr[1] == 'cpy') {
      if (instr[2] %in% letters) {
        registers[instr[3]] <- registers[instr[2]]
      } else {
        registers[instr[3]] <- as.numeric(instr[2])
      }
      
    } else if (instr[1] == 'inc') {
      registers[instr[2]] <- registers[instr[2]] + 1
      
    } else if (instr[1] == 'dec') {
      registers[instr[2]] <- registers[instr[2]] - 1
      
    } else if (instr[1] == 'jnz') {
      if (instr[2] %in% letters) {
        if (registers[instr[2]] != 0) {
          i <- i + as.numeric(instr[3]) - 1
        }
      } else if (instr[2] != '0') {
        i <- i + as.numeric(instr[3]) - 1
      }
    }
    i = i + 1
  }
  return(registers['a'])
}
execute_assembunny(input, registers)

# Part 2
execute_assembunny(input, registers + c(0, 0, 1, 0))
