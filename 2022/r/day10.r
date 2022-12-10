# --- Day 10: Cathode-Ray Tube ---
input <- readLines("2022/data/input10.txt") |>
  strsplit(' ')

# Part 1
cycles <- c(1)
for (i in seq(input)) {
  if (input[[i]][1] == 'noop') cycles <- c(cycles, tail(cycles, 1))
  if (input[[i]][1] == 'addx') cycles <- c(cycles, tail(cycles, 1) + c(0, as.numeric(input[[i]][2])))
}
sapply(seq(20, length(cycles), 40), \(i) cycles[i] * i) |>
  sum()

# Part 2
pos <- (seq(cycles) - 1) %% 40
draw <- abs(pos - cycles) <= 1
img <- matrix(head(draw, -1), ncol = 40, byrow = T) 
image(t(img[nrow(img):1,]))
