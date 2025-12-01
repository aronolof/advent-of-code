#--- Day 5: Print Queue ---
input <- readLines('2024/data/test05.txt') 

ordering <- input[seq(which(input == '') - 1)] |>
  strsplit('\\|')

pages <- input[seq_along(input) > which(input == '')] |>
  strsplit(',')

x <- pages[[1]]
sapply(ordering, c)

x
