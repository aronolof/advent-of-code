# --- Day 3: Spiral Memory ---

# Part 1
required_steps <- function(x){
  if(x == 0) return(0)
  minimum_step <- ceiling(sqrt(x)) %/% 2
  minimum_step + abs((((x)-1) %% (2 * minimum_step)) - minimum_step)
}
required_steps(368078)
