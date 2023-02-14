# --- Day 12: JSAbacusFramework.io ---

input <- readLines('2015/data/input12.txt')

# Part 1
sum_of_numbers <- function(input) {
  gsub('[^0-9-]+', ',', input) |>
    strsplit(',') |>
    unlist() |>
    as.numeric() |>
    sum(na.rm = TRUE)
}

sum_of_numbers(input)

# Part 2
ignore_red <- function(input) {
  chars <- strsplit(input, '')[[1]]
  bracket <- c(']' = '[', '}' = '{')
  stack <- c()
  backtracking <- c()
  for (char in chars) {
    stack <- append(stack, char)
    
    if (char %in% names(bracket)) {
      while (TRUE) {
        backtracking <- append(backtracking, tail(stack, 1))
        stack <- head(stack, -1)
        if (tail(backtracking, 1) == bracket[char]) {
          break
        }
      }
      backtracking <- paste(rev(backtracking), collapse = '')
      if (char == ']' | !grepl('red', backtracking)) {
        stack <- append(stack, gsub('red', '', backtracking))
      }
      backtracking <- c()
    }
  }
  sum_of_numbers(stack)
}

ignore_red(input)
