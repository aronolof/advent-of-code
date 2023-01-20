# --- Day 5: Sunny with a Chance of Asteroids ---

# My brand new Intcode computer
run_prog <- function(prog, input) {
  i <- 1
  
  while (i <= length(prog)) {
    
    op <- prog[i] %% 100
    mode <- sapply(2:4, \(x) (prog[i] %% (10^(x + 1))) %/% (10^x))
    
    par <- function(n) {
      ifelse(mode[n], prog[i + n], prog[prog[i + n] + 1])
    }
    
    if (op %in% c(1:2, 7:8)) {
      # params = 3
      prog[prog[i + 3] + 1] <- do.call(c("sum", "prod", "<", "==")[[which(op == c(1,2,7,8))]],
                                       list(par(1), par(2)))
      if (prog[i + 3] + 1 == i) {
        i <- prog[i + 3] + 1
      } else {
        i <- i + 4
      }
      
    } else if (op == 3) {
      # params = 1
      prog[prog[i + 1] + 1] <- input
      if (prog[i + 1] + 1 == i) {
        i <- prog[i + 1] + 1
      } else {
        i <- i + 2
      }
      
    } else if (op == 4) {
      # params = 1
      print(ifelse(mode[1], prog[i + 1], prog[prog[i + 1] + 1]))
      i <- i + 2
      
    } else if (op %in% 5:6) {
      # params = 2
      if (do.call(c("!=", "==")[[which(op == c(5:6))]], list(par(1), 0))) {
        i <- par(2) + 1
      } else {
        i <- i + 2 + 1
      }
      
    } else if (op == 99) {
      break
    } else {
      stop()
    }
  }
}

input <- scan("2019/input/input-05.txt", sep = ",")

# Part 1
run_prog(input, 1)

# Part 2
run_prog(input, 5)
