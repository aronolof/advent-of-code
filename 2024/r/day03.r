#--- Day 3: Mull It Over ---
input <- readLines('2024/data/input03.txt') |>
  paste(collapse = '')

# Part 1
mull_it_over <- function(input) {
  regex <- "mul\\((\\d+,\\d+)\\)"
  gsub(regex, "\\1", regmatches(input, gregexpr(regex, input))[[1]]) |>
    strsplit(',') |>
    sapply(\(x) prod(as.integer(x))) |>
    sum()
}

mull_it_over(input)

# Part 2
enabled <- gsub('don\'t\\(\\).*?do\\(\\)', '', input)
enabled <- gsub('don\'t\\(\\).*', '', enabled) # The last one is not re-enabled

mull_it_over(enabled)
