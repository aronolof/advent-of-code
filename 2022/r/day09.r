# --- Day 8: Treetop Tree House ---
input <- readLines("2022/data/input09.txt") |>
  strsplit(' ') 

# Part 1
hpos <- c(0, 0)
tpos = c(0, 0)
visited <- list(tpos)

for (move in input) {
  for (step in seq(as.numeric(move[2]))) {
    if (move[1] == 'R') hpos = hpos + c(1, 0)
    if (move[1] == 'L') hpos = hpos + c(-1, 0)
    if (move[1] == 'U') hpos = hpos + c(0, -1)
    if (move[1] == 'D') hpos = hpos + c(0, 1)
    
    if (max(abs(tpos - hpos)) > 1) tpos = tpos + sign(hpos - tpos)
    visited <- append(visited, list(tpos))
  }
}
length(unique(visited))

## Part 2
hpos <- c(0, 0)
tpos = c(0, 0)
visited <- list(tpos)

pos1 <- c(0, 0)
pos2 <- c(0, 0)
pos3 <- c(0, 0)
pos4 <- c(0, 0)
pos5 <- c(0, 0)
pos6 <- c(0, 0)
pos7 <- c(0, 0)
pos8 <- c(0, 0)

for (move in input) {
  for (step in seq(as.numeric(move[2]))) {
    if (move[1] == 'R') hpos = hpos + c(1, 0)
    if (move[1] == 'L') hpos = hpos + c(-1, 0)
    if (move[1] == 'U') hpos = hpos + c(0, -1)
    if (move[1] == 'D') hpos = hpos + c(0, 1)
    
    if (max(abs(pos1 - hpos)) > 1) pos1 = pos1 + sign(hpos - pos1)
    if (max(abs(pos2 - pos1)) > 1) pos2 = pos2 + sign(pos1 - pos2)
    if (max(abs(pos3 - pos2)) > 1) pos3 = pos3 + sign(pos2 - pos3)
    if (max(abs(pos4 - pos3)) > 1) pos4 = pos4 + sign(pos3 - pos4)
    if (max(abs(pos5 - pos4)) > 1) pos5 = pos5 + sign(pos4 - pos5)
    if (max(abs(pos6 - pos5)) > 1) pos6 = pos6 + sign(pos5 - pos6)
    if (max(abs(pos7 - pos6)) > 1) pos7 = pos7 + sign(pos6 - pos7)
    if (max(abs(pos8 - pos7)) > 1) pos8 = pos8 + sign(pos7 - pos8)
    if (max(abs(tpos - pos8)) > 1) tpos = tpos + sign(pos8 - tpos)
    
    visited <- append(visited, list(tpos))
  }
}
length(unique(visited))

