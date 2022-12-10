# --- Day 10: Cathode-Ray Tube ---
input <- readLines("2022/data/input10.txt") |>
  strsplit(' ')

# Part 1
cycle <- 1
register <- 1
for (i in seq(input)) {
  if (input[[i]][1] == 'noop') {
    cycle <- cycle + 1
    register[cycle] <- tail(register, 1)
  } else if (input[[i]][1] == 'addx') {
    cycle <- cycle + 2
    register[cycle + c(-1, 0)] <- tail(register, 1) + c(0, as.numeric(input[[i]][2]))
  }
}
sapply(seq(20, length(register), 40), \(i) register[i] * i) |>
  sum()

# Part 2
pos <- (seq(register) - 1) %% 40
draw <- abs(pos - register) <= 1
img <- matrix(head(draw, -1), ncol = 40, byrow = T) 
image(t(img[nrow(img):1,]))
