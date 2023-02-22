# --- Day 13: Care Package ---

# Part 1
run_program <- function(program, input = 1){
  
  get_param <- function(param) {
    mode <- (x[n] %/% 10^(param + 1)) %% 10
    if (mode == 0) return(x[n + param])
    if (mode == 1) return(n + param - 1)
    if (mode == 2) return(x[n + param] + relative_base)
  }
  
  x <- program
  n <- 1
  relative_base <- 0
  output <- c()
  
  while (x[n] != 99) {
    op <- x[n] %% 100
    p1 <- x[get_param(1) + 1]
    p2 <- x[get_param(2) + 1]
    p3 <- get_param(3) + 1
    
    if (op %in% c(1:2, 7:8)) {
      if (op == 1) x[p3] <- p1 + p2
      if (op == 2) x[p3] <- p1 * p2
      if (op == 7) x[p3] <- p1 < p2
      if (op == 8) x[p3] <- p1 == p2
      n <- n + 4
      
    } else if (op == 3) {
      x[p3] <- input
      n <- n + 2
      
    } else if (op == 4) {
      output <- append(output, p1)
      n <- n + 2
      
    } else if (op %in% 5:6) {
      n <- ifelse((op == 5 && p1) | op == 6 && !p1, p2 + 1, n + 3)
      
    } else if (op == 9) {
      relative_base <- relative_base + p1
      n <- n + 2
    }
  }
  return(output)
}

program <- scan("2019/input/input-13.txt", sep = ",")
result <- run_program(program)
result[(seq(result) %% 3 == 0) & (result == 2)] |>
  length()

# Part 2
