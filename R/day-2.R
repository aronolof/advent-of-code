# Description

input <- scan("input-day-2.txt", sep = ",")

# Part 1
run_prog <- function(input, replace1, replace2) {
  prog <- input
  prog[2:3] <- c(replace1, replace2)
  for (i in seq(1, length(prog), 4)) {
    if(prog[i] == 99) {
      return(prog[1])
    } else {
      prog[prog[i + 3] + 1] <- do.call(c("sum", "prod")[prog[i]], list(prog[prog[i + 1:2] + 1]))
    }
  }
}

run_prog(input, 12, 2)

# Part 2
for (noun in 0:99) {
  for (verb in 0:99) {
    if (run_prog(input, noun, verb) == 19690720) {
      print(sum(c(noun, verb)* c(100, 1)))
      break
    }
  }
}

# Alternatively:
#grid <- expand.grid(noun = 0:99, verb = 0:99)
#sum(grid[which(mapply(run_prog, list(input), grid$noun, grid$verb) == 19690720),] * c(100, 1))


