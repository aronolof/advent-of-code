# --- Day 17: No Such Thing as Too Much ---

input <- scan('2015/data/input17.txt')

find_combinations <- function(containers = input, fit = 150) {
  containers <- containers |>
    sort(decreasing = TRUE)
  
  try_combinations <- function(containers, amount) {
    if (amount == fit) {
      return(1)
    }
    if (length(containers) <= 0 || amount > fit) return(0)
    
    combinations <- 0
    for (i in seq_along(containers)) {
      combinations <- combinations + try_combinations(tail(containers, -i), amount + containers[i])
    }
    return(combinations)
  }
  
  try_combinations(containers, amount = 0)
}

# Part 1
find_combinations(input, 150)
