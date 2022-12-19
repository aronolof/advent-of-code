# --- Day 2: Bathroom Security ---
input <- readLines("2016/data/input02.txt") |>
  strsplit('')

# Part 1
bathroom_code <- function(input) {
  code <- 0
  keypad <- matrix(1:9, nrow = 3, byrow = T)
  dirs <- c('R' = 1, 'L' = -1, 'D' = 1i, 'U' = -1i)
  pos <- 2 + 2i
  
  for (j in seq(input)) {
    for (i in seq(input[[j]])) {
      value <- dirs[input[[j]][i]]
      pos <- pmax(pmin(3, Re(pos + value)), 1) + 1i * pmax(pmin(3, Im(pos + value)), 1)
    }
    code <- code + keypad[Im(pos), Re(pos)] * 10^(j - 1)
  }
  return(code)
}

bathroom_code(input)

# Part 2
bathroom_code_pt2 <- function(input) {
  code <- rep(NA, length(input))
  keypad <- matrix(
    c(NA, NA, 1, NA, NA, NA, 2:4, NA, 5:9, NA, c('A', 'B', 'C'), NA, NA, NA, 'D', NA, NA),
    nrow = 5, byrow = T
  )
  dirs <- c('R' = 1, 'L' = -1, 'D' = 1i, 'U' = -1i)
  pos <- 3 + 3i
  
  for (j in seq(input)) {
    for (i in seq(input[[j]])) {
      value <- dirs[input[[j]][i]]
      attempt_pos <- pmax(pmin(5, Re(pos + value)), 1) + 1i * pmax(pmin(5, Im(pos + value)), 1)
      
      if (!is.na(keypad[Im(attempt_pos), Re(attempt_pos)])) {
        pos <- attempt_pos
      }
    }
    code[j] <- keypad[Im(pos), Re(pos)]
  }
  return(paste(code, collapse = ''))
}

bathroom_code_pt2(input)
