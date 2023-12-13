# --- Day 12: Hot Springs ---
input <- readLines("2023/data/input12.txt")

cond <- gsub('[?.# ]', '', input) |>
  strsplit(',') |>
  sapply(as.numeric)

springs <- gsub('[^?.#]', '', input) |>
  strsplit('')

# Part 1
hotsprings <- function(x, co, score = 0) {
  q_pos <- which(x == '?')
  rle_x <- rle(x)
  rle_x_lengths <- rle_x$lengths[rle_x$values == '#']
  rle_x_values <- rle_x$values[rle_x$values == '#']
  
  # First, check if there are no more question marks. Then return the final verdict
  if (length(q_pos) == 0) {
    #print('reached end')
    if (length(co) == length(rle_x_lengths) && all(rle_x_lengths == co)) {
      return(1)
    } else {
      return(0)
    }
  }

  # Check if the arrangement works so far. If not, we don't need to continue
  pos_to_check <- rle_x$lengths[rle_x$values == '#' & (cumsum(rle_x$values == '?') == 0)]

  if(length(pos_to_check) > length(co)) return(0)
  
  if (length(pos_to_check) == 0) { 
    # The first letter is a question mark. Just continue.
  } else if (all(pos_to_check <= co[0:length(pos_to_check)])) {
    # Arrangement matches so far. Continue.
  } else {
    # Arrangement won't fit. Quit.
    return(0)
  }

  # Replace the next question mark and check
  attempt <- x
  attempt[q_pos[1]] <- '#'
  tally <- hotsprings(attempt, co, score)

  attempt <- x
  attempt[q_pos[1]] <- '.'
  tally <- tally + hotsprings(attempt, co, score)

  return(tally)
}

total <- 0
for(i in 1:1000){
  total <- total + hotsprings(springs[[i]], cond[[i]])
}
total

# Part 2

springs2 <- gsub('[^?.#]', '', input) |>
  sapply(\(x) {
    strsplit(paste0(rep(x, 2), collapse = '?'), '')[[1]]
  }) |>
  unname()

cond2 <- sapply(cond, rep, time = 5)


