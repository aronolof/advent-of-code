# --- Day 2: Rock Paper Scissors ---

input <- read.table("2022/data/input02.txt", sep=" ")

# Part 1
apply(input, 1, \(x) {
  
    opponent = setNames(1:3, c('A','B','C'))[x[1]]
    choice   = setNames(1:3, c('X','Y','Z'))[x[2]]
    
    c(3, 6, 0)[1 + (choice - opponent) %% 3] + choice
  }) |>
  sum()

# Part 2
apply(input, 1, \(x) {
  
    opponent = setNames(1:3, c('A','B','C'))[x[1]]
    strategy = setNames(1:3, c('X','Y','Z'))[x[2]]
    choice = 1 + (opponent + strategy) %% 3
    
    c(0, 3, 6)[strategy] + choice
  }) |>
  sum()
