# --- Day 5: Sunny with a Chance of Asteroids ---
input <- scan("2019/input/input-05.txt", sep = ",")

# My brand new Intcode computer
run_prog <- function(prog, input) {
  
  par <- function(n) {
    if (mode[n] == 0) return(prog[prog[i + n] + 1])
    if (mode[n] == 1) return(prog[i + n])
  }
  
  i <- 1
  
  while (i <= length(prog)) {
    
    op <- prog[i] %% 100
    mode <- sapply(2:4, \(x) (prog[i] %% (10^(x + 1))) %/% (10^x))
    
    if (op %in% c(1:2, 7:8)) {
      # n_params: 3
      prog[prog[i + 3] + 1] <- do.call(c("sum", "prod", "<", "==")[[which(op == c(1,2,7,8))]],
                                       list(par(1), par(2)))
      i <- i + 4
      
    } else if (op == 3) {
      # n_params: 1
      prog[prog[i + 1] + 1] <- input
      i <- i + 2
      
    } else if (op == 4) {
      # n_params: 1
      if (mode[1] == 0) print(prog[prog[i + 1] + 1])
      if (mode[1] == 1) print(prog[i + 1])
      i <- i + 2
      
    } else if (op %in% 5:6) {
      # n_params: 2
      if (do.call(c("!=", "==")[[which(op == c(5:6))]], list(par(1), 0))) {
        i <- par(2) + 1
      } else {
        i <- i + 2 + 1
      }
      
    } else if (op == 99) {
      break
    } else {
      stop("Opcode error")
    }
  }
}

# Part 1
run_prog(input, 1)

# Part 2
run_prog(input, 5)
