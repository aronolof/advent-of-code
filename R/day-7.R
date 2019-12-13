library(tidyverse)

# My ol' trusty Intcode computer
run_prog <- function(settings) {
  
  i <- settings$i
  prog <- settings$prog
  input <- settings$input
  output <- settings$output
  
  while(i <= length(prog)){
    
    op <- prog[i] %% 100
    mode <- rev(as.numeric(str_split(str_pad((prog[i] %/% 100), width = 3, pad = 0),
                                     "",
                                     simplify = TRUE)))
    
    par <- function(n) {
      ifelse(mode[n], prog[i + n], prog[prog[i + n] + 1])
    }
    
    if (op %in% c(1:2, 7:8)) {
      # params = 3
      prog[prog[i + 3] + 1] <- do.call(c("sum", "prod", "<", "==")[[which(op == c(1,2,7,8))]],
                                       list(par(1), par(2)))
      ifelse(prog[i + 3] + 1 == i, i <- prog[i + 3] + 1, i <- i + 4)
      
    } else if (op == 3) {
      # params = 1
      prog[prog[i + 1] + 1] <- input[1]
      input <- input[-1]
      ifelse(prog[i + 1] + 1 == i, i <- prog[i + 1] + 1, i <- i + 2)
      
    } else if (op == 4) {
      # params = 1
      settings <- list(input = input,
                       prog = prog, i = i+2,
                       output = c(ifelse(mode[1], prog[i + 1], prog[prog[i + 1] + 1])),
                       halt = FALSE)
      return(settings)
      
    } else if (op %in% 5:6) {
      # params = 2
      ifelse(do.call(c("!=", "==")[[which(op == c(5:6))]], list(par(1), 0)),
             i <- par(2) + 1,
             i <- i + 2 + 1)
      
    } else if (op == 99) {
      return(list(output = output, halt = TRUE))
    } else {
      print("error")
      break
    }
  }
}

prog <- scan("input/7.txt", sep = ",")

# Part 1
run_prog_outer <- function(arg1, arg2) {
  run_prog(settings = list(input = c(arg1, arg2), prog = prog, i = 1, output = c()))$output
}
df <- expand.grid(rep(list(0:4), 5)) %>%
  filter(apply(., 1, function(x) length(unique(x))) == ncol(.))
output <- 0
for(i in 1:5) {
  output <- mapply(run_prog_outer, df[[i]], output)
}
max(output)


# Part 2 (to be refactored...)

amp_loop <- function(x1, x2, x3, x4, x5) {
  amp1 <- list(input = x1, prog = prog, i = 1, output = c())
  amp2 <- list(input = x2, prog = prog, i = 1, output = c())
  amp3 <- list(input = x3, prog = prog, i = 1, output = c())
  amp4 <- list(input = x4, prog = prog, i = 1, output = c())
  amp5 <- list(input = x5, prog = prog, i = 1, output = c(0), halt = FALSE)
  
  while(amp5$halt == FALSE) {
    amp1$input[length(amp1$input)+1] <- amp5$output
    amp1 <- run_prog(amp1)
    
    amp2$input[length(amp2$input)+1] <- amp1$output
    amp2 <- run_prog(amp2)
    
    amp3$input[length(amp3$input)+1] <- amp2$output
    amp3 <- run_prog(amp3)
    
    amp4$input[length(amp4$input)+1] <- amp3$output
    amp4 <- run_prog(amp4)
    
    amp5$input[length(amp5$input)+1] <- amp4$output
    amp5 <- run_prog(amp5)
  }
  return(amp5$output)
}

df <- expand.grid(rep(list(5+0:4), 5)) %>%
  filter(apply(., 1, function(x) length(unique(x))) == ncol(.))

mapply(feedback_loop, df[[1]], df[[2]], df[[3]], df[[4]], df[[5]]) %>%
  max()
