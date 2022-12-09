# --- Day 8: Treetop Tree House ---
input <- readLines("2022/data/input09.txt") |>
  strsplit(' ')

# Part 1
hpos <- c(0, 0)
tpos <- c(0, 0)
visited <- list(tpos)

for (move in input) {
  for (step in seq(as.numeric(move[2]))) {
    if (move[1] == 'R') hpos <- hpos + c(1, 0)
    if (move[1] == 'L') hpos <- hpos + c(-1, 0)
    if (move[1] == 'U') hpos <- hpos + c(0, -1)
    if (move[1] == 'D') hpos <- hpos + c(0, 1)
    
    if (max(abs(tpos - hpos)) > 1) tpos <- tpos + sign(hpos - tpos)
    
    visited <- append(visited, list(tpos))
  }
}
length(unique(visited))

## Part 2
rope <- rep(list(c(0, 0)), 10)
visited <- rope[10]

for (move in input) {
  for (step in seq(as.numeric(move[2]))) {
    if (move[1] == 'R') rope[[1]] <- rope[[1]] + c(1, 0)
    if (move[1] == 'L') rope[[1]] <- rope[[1]] + c(-1, 0)
    if (move[1] == 'U') rope[[1]] <- rope[[1]] + c(0, -1)
    if (move[1] == 'D') rope[[1]] <- rope[[1]] + c(0, 1)
    
    for (i in seq(rope)[-1]) {
      if (max(abs(rope[[i]] - rope[[i - 1]])) > 1) {
        rope[[i]] <- rope[[i]] + sign(rope[[i - 1]] - rope[[i]])
      }
    }
    visited <- append(visited, rope[10])
  }
}
length(unique(visited))
