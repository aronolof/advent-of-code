# --- Day 14: Chocolate Charts ---
input <- 880751

# Part 1
part_1 <- function(input) {
  # Setup
  recipes <- rep(NA, input + 10)
  recipes[1:2] <- c(3, 7)
  n_recipes <- 2
  elf_1 <- 1
  elf_2 <- 2
  
  while (n_recipes < length(recipes)) {
    sum_recipes <- recipes[elf_1] + recipes[elf_2]
    new_recipes <- as.numeric(strsplit(as.character(sum_recipes), '')[[1]])
    recipes[n_recipes + seq_along(new_recipes)] <- new_recipes
    n_recipes <- n_recipes + length(new_recipes)
    
    elf_1 <- ((elf_1 + recipes[elf_1]) %% n_recipes) + 1
    elf_2 <- ((elf_2 + recipes[elf_2]) %% n_recipes) + 1
  }
  
  recipes[input + (1:10)] |>
    paste(collapse = '') |>
    as.numeric()
}

part_1(input)

# Part 2
part_2 <- function(input) {
  # Setup
  recipes <- c(3, 7)
  n_recipes <- 2
  elf_1 <- 1
  elf_2 <- 2
  
  target <- as.numeric(strsplit(as.character(input), '')[[1]])
  
  while (TRUE) {
    sum_recipes <- recipes[elf_1] + recipes[elf_2]
    new_recipes <- as.numeric(strsplit(as.character(sum_recipes), '')[[1]])
    recipes[n_recipes + seq_along(new_recipes)] <- new_recipes
    n_recipes <- n_recipes + length(new_recipes)
    
    elf_1 <- ((elf_1 + recipes[elf_1]) %% n_recipes) + 1
    elf_2 <- ((elf_2 + recipes[elf_2]) %% n_recipes) + 1
    
    for (i in seq_along(new_recipes) - 1) {
      if ((n_recipes - length(target) + 1 - i) >= 1) {
        if (all(target == recipes[((n_recipes - length(target) + 1):n_recipes) - i])) {
          return(n_recipes - length(target) - i)
        }
      }
    }
    if ((n_recipes %% 1000) == 0) print(length(recipes))
  }
}

part_2(input)
