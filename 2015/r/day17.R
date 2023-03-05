# --- Day 17: No Such Thing as Too Much ---

input <- scan('2015/data/input17.txt')

find_combinations <- function(containers = input, fit = 150, part_2 = FALSE) {
  containers <- containers |>
    sort(decreasing = TRUE)
  
  combinations <- list()
  
  try_combinations <- function(containers, used) {
    if (sum(used) == fit) {
      if (part_2) {
        combinations <<- append(combinations, list(used))
      }
      return(1)
    }
    if (length(containers) <= 0 || sum(used) > fit) return(0)
    
    n_combinations <- 0
    for (i in seq_along(containers)) {
      n_combinations <- n_combinations + try_combinations(tail(containers, -i), c(used, containers[i]))
    }
    return(n_combinations)
  }
  
  n_combinations <- try_combinations(containers, NULL)
  
  if (part_2) return(combinations)
  
  return(n_combinations)
  
}

# Part 1
find_combinations(input, 150)

# Part 2
find_combinations(input, 150, part_2 = TRUE) |>
  sapply(length) |>
  table() |>
  head(1)
