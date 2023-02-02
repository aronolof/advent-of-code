# --- Day 19: A Series of Tubes ---
input <- readLines('2017/data/input19.txt') |>
  strsplit('') |>
  sapply(\(x) x) |>
  t()

# Part 1
s <- c(1, which(input[1,] != ' '))
d <- c(1, 0)
i <- 0
password <- c()

while (TRUE) {
  current <- input[s[1], s[2]]
  
  if (current %in% c('-', '|')) {
    
  } else if (current %in% LETTERS) {
    password <- append(password, current)
  } else if (current == '+') {
    next_step <- input[(s + d)[1], (s + d)[2]]
    
    if (abs(d[1]) == 1 && next_step %in% c('-', ' ')) {
      turn <- which(!(input[s[1], s[2] + c(-1, 1)] %in% c(' ', '|')))
      if (turn == 1) d <- c(0, -1)
      if (turn == 2) d <- c(0, 1)
    } else if (abs(d[1]) == 0 && next_step %in% c('-', ' ')) {
      turn <- which(!(input[s[1] + c(-1, 1), s[2]] %in% c(' ', '-')))
      if (turn == 1) d <- c(-1, 0)
      if (turn == 2) d <- c(1, 0)
    }
  } else {
    break
  }
  s <- s + d
  i <- i + 1
}
paste(password, collapse = '')

# Part 2
i
