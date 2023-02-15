# --- Day 9: Sensor Boost ---
program <- scan("2019/input/input-09.txt", sep = ",")

# My brand new Intcode computer
run_prog <- function(prog, input = 0) {
  relative_base = 0
  i <- 1
  
  while (TRUE) {
    op <- prog[i] %% 100
    mode <- sapply(2:4, \(x) (prog[i] %% (10^(x + 1))) %/% (10^x))
    
    par <- function(n) {
      if (mode[n] == 0) {
        val <- prog[prog[i + n] + 1]
      } else if (mode[n] == 1) {
        val <- prog[i + n]
      } else if (mode[n] == 2) {
        val <- prog[prog[i + n] + 1 + relative_base]
      } else {
        stop("Mode error")
      }
      ifelse(is.na(val), 0, val)
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
      if (mode[1] == 0) prog[prog[i + 1] + 1] <- input
      if (mode[1] == 2) {
        print(i)
        print(op)
        print(prog[i])
        print(prog[i + 1])
        prog[prog[i + 1] + 1 + relative_base] <- input
        stop("STOP")
      }
      
      if (prog[i + 1] + 1 == i) {
        i <- prog[i + 1] + 1
      } else {
        i <- i + 2
      }
      
    } else if (op == 4) {
      # params = 1
      if (mode[1] == 0) print(as.character(prog[prog[i + 1] + 1]))
      if (mode[1] == 1) print(as.character(prog[i + 1]))
      if (mode[1] == 2) print(as.character(prog[prog[i + 1] + 1 + relative_base]))

      i <- i + 2
      
    } else if (op %in% 5:6) {
      # params = 2
      if (do.call(c("!=", "==")[[which(op == c(5:6))]], list(par(1), 0))) {
        i <- par(2) + 1
      } else {
        i <- i + 2 + 1
      }
    } else if (op == 9) {
      # params = 1
      relative_base <- relative_base + par(1)
      i <- i + 2
    } else if (op == 99) {
      break
    } else {
      stop("Invalid opcode")
    }
  }
}

c(104,1125899906842624,99) |>
  run_prog()

# Part 1
run_prog(program, 1)

# Part 2
run_prog(input, 5)
