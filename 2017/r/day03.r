# --- Day 3: Spiral Memory ---
input <- 368078
  
# Part 1
required_steps <- function(x){
  if (x == 0) return(0)
  minimum_step <- ceiling(sqrt(x)) %/% 2
  minimum_step + abs((((x) - 1) %% (2 * minimum_step)) - minimum_step)
}
required_steps(input)

# Part 2
find_first_value <- function(input) {
  val <- 1
  pos_x <- 0
  pos_y <- 0
  
  i <- 1
  while (TRUE) {
  
    y <- rep(c(0, 1) * ifelse(i %% 2, -1, 1), each = i)
    x <- rep(c(1, 0) * ifelse(i %% 2, 1, -1), each = i)
    
      for (n in seq(i * 2)) {
        pos_x <- append(pos_x, tail(pos_x, 1) + x[n])
        pos_y <- append(pos_y, tail(pos_y, 1) + y[n])
        
        neighbor <- abs(tail(pos_x, 1) - pos_x) %in% (-1:1) &
          abs(tail(pos_y, 1) - pos_y) %in% (-1:1)
        
        val <- append(val, sum(head(val[neighbor], -1)))
        
        if (tail(val, 1) > input) {
          return(tail(val, 1))
        }
      }
    i <- i + 1
  }
}

find_first_value(input)



