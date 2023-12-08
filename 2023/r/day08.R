# --- Day 7: Camel Cards ---

input <- readLines("2023/data/input08.txt")

# Part 1
nodes <- input[-(1:2)] |>
  substr(8, 15) |>
  strsplit(', ')

names(nodes) <- substr(input, 1, 3)[-(1:2)]
steps <- (strsplit(input[1], '')[[1]] == 'R') + 1

i <- 0
pos <- 'AAA'

while (TRUE) {
  j <- (i %% length(steps)) + 1
  pos <- a[[pos]][steps[j]]
  i <- i + 1
  if(pos == 'ZZZ') break
}
i
