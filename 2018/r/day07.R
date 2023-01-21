# --- Day 7: The Sum of Its Parts ---

parse_input <- function(input_path) {
  input <- readLines(input_path) |>
    strsplit(' ') |>
    sapply(\(x) x[c(2, 8)])
  
  step_names <- unique(c(input))
  steps <- lapply(step_names, \(x) list(FALSE, input[1,][input[2,] == x]))
  steps <- lapply(step_names, \(x) input[1,][input[2,] == x])
  names(steps) <- step_names
  
  return(steps)
}

# Part 1
part_1 <- function(steps) {
  letter_order <- c()
  while (length(steps) > 0) {
    next_step <- sapply(steps, length) == 0
    next_step <- sort(names(next_step[next_step]))[1]
    
    letter_order <- append(letter_order, next_step)
    
    steps[next_step] <- NULL
    steps <- lapply(steps, \(x) x[x != next_step])
  }
  return(paste(letter_order, collapse = ''))
}

parse_input('2018/data/input07.txt') |>
  part_1()

# Part 2
part_2 <- function(steps, time_add = 60, n_workers = 5) {
  worker <- rep('.', n_workers)
  step_time_left <- setNames(seq(LETTERS) + time_add, LETTERS)
  letter_order <- c()
  second <- 0
  
  while (length(steps) > 0) {
    
    available_steps <- sapply(steps, length) == 0
    available_steps <- sort(names(available_steps[available_steps]))
    
    for (a in available_steps) {
      if (any(worker == '.') && !(a %in% worker)) {
        worker[worker == '.'][1] <- a
      }
    }
    
    step_time_left[worker] <- step_time_left[worker] - 1
    ready_steps <- sort(names(step_time_left[worker][step_time_left[worker] == 0]))
    
    for (next_step in ready_steps) {
      letter_order <- append(letter_order, next_step)
      steps[next_step] <- NULL
      steps <- lapply(steps, \(x) x[x != next_step])
      worker[worker == next_step] <- '.'
    }
    second <- second + 1
  }
  return(second)
}

parse_input('2018/data/input07.txt') |>
  part_2()
