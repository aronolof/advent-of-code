# --- Day 8: Handheld Halting ---

input <- read.table("2020/input/input-08.txt", sep = "")

# Part 1
run_program <- function(input) {
  input$V3 <- FALSE
  accumulator = 0
  i = 1
  
  while (TRUE) {
    if(i > nrow(input)) return(c(accumulator, loop = FALSE))
    if(input[i,3]) return(c(accumulator, loop = TRUE))
    input[i,3] <- TRUE
    accumulator <- accumulator + (input[i,1] == "acc") * input[i,2]
    i <- i + 1 + (input[i,1] == "jmp") * (input[i,2]-1)
  }
  
}

run_program(input)[[1]]

# Part 2
programs <- lapply(which(input$V1 %in% c('jmp', 'nop')), function(x) {
  input[x,1] <- c('jmp', 'nop')[(input[x,1] == "jmp") + 1]
  run_program(input)
})

programs[[which(sapply(programs, function(x) x[[2]] == 0))]][1]







