library(stringr)

input <- readLines("2022/data/input11.txt") 
sep <- cumsum(grepl('Monkey', input))

state = split(input, sep)

# Prep
for (i in seq(state)) {
  state[[i]][7] <- list(state[[i]][2] |>
    str_replace('  Starting items: ', '') |>
    str_split(',') |>
    sapply(as.numeric, simplify = T) |>
    as.vector())
  
  state[[i]][3] <- str_replace(state[[i]][3], '  Operation: new = ', '')
  
  state[[i]][4] <- str_replace(state[[i]][4], '  Test: divisible by ', '') |>
    as.integer()
  
  state[[i]][5] <- str_replace(state[[i]][5], '    If true: throw to monkey ', '') |>
    as.integer()
  
  state[[i]][6] <- str_replace(state[[i]][6], '    If false: throw to monkey ', '') |>
    as.integer()
  
  state[[i]][1]  <- 0
}

# Monkey business
for (round in 1:20) {
  for (i in seq(state)) {
    
    if (length(state[[i]][7][[1]]) > 0) {
      for (j in seq(length(state[[i]][7][[1]]))) {
        
        item = state[[i]][[7]][j]
        new_worry <- str_replace_all(state[[i]][[3]], 'old', as.character(item)) |>
          parse(text = _) |>
          eval()
        
        new_worry <- new_worry %/% 3
        is_divisible <- (new_worry %% state[[i]][[4]]) == 0
        
        if (is_divisible) {
          throw_to <- state[[i]][[5]] + 1
          state[[throw_to]][7][[1]] <- append(state[[throw_to]][7][[1]], new_worry)
        } else {
          throw_to <- state[[i]][[6]] + 1
          state[[throw_to]][7][[1]] <- append(state[[throw_to]][7][[1]], new_worry)
        }
        state[[i]][[1]] <- state[[i]][[1]] + 1
      }
      state[[i]][[7]] <- c()
    }

  }
}

# Part 2
sapply(seq(state), \(i) state[[i]][[1]]) |>
  sort(decreasing = T) |> head(2) |> prod()

