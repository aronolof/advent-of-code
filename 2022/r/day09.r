# --- Day 8: Treetop Tree House ---
input <- readLines("2022/data/input09.txt")
steps = strsplit(input, ' ')

# Part 1
hposframe <- data.frame(x = 0, y = 0)
hpos <- c(0, 0)
tposframe <- data.frame(x = 0, y = 0)
tpos = c(0, 0)

for (x in steps) {
  #x = steps[[1]]
  for (i in seq(as.numeric(x[2]))) {
    if (x[1] == 'R') hpos = hpos + c(1,0)
    if (x[1] == 'L') hpos = hpos + c(-1,0)
    if (x[1] == 'U') hpos = hpos + c(0,-1)
    if (x[1] == 'D') hpos = hpos + c(0,1)
    
    hposframe = rbind(hposframe, hpos)
      
    if (max(abs(tpos - hpos)) >= 2) tpos = tpos + sign(hpos - tpos)
    tposframe = rbind(tposframe, tpos)
  }
}
nrow(unique(tposframe))

## Part 2
hposframe <- data.frame(x = 0, y = 0)
hpos <- c(0, 0)
tposframe <- data.frame(x = 0, y = 0)
tpos = c(0, 0)

pos1 <- c(0, 0)
pos2 <- c(0, 0)
pos3 <- c(0, 0)
pos4 <- c(0, 0)
pos5 <- c(0, 0)
pos6 <- c(0, 0)
pos7 <- c(0, 0)
pos8 <- c(0, 0)

for (x in steps) {
  #x = steps[[1]]
  for (i in seq(as.numeric(x[2]))) {
    if (x[1] == 'R') hpos = hpos + c(1,0)
    if (x[1] == 'L') hpos = hpos + c(-1,0)
    if (x[1] == 'U') hpos = hpos + c(0,-1)
    if (x[1] == 'D') hpos = hpos + c(0,1)
    
    hposframe = rbind(hposframe, hpos)
    
    if (max(abs(pos1 - hpos)) >= 2) pos1 = pos1 + sign(hpos - pos1)
    if (max(abs(pos2 - pos1)) >= 2) pos2 = pos2 + sign(pos1 - pos2)
    if (max(abs(pos3 - pos2)) >= 2) pos3 = pos3 + sign(pos2 - pos3)
    if (max(abs(pos4 - pos3)) >= 2) pos4 = pos4 + sign(pos3 - pos4)
    if (max(abs(pos5 - pos4)) >= 2) pos5 = pos5 + sign(pos4 - pos5)
    if (max(abs(pos6 - pos5)) >= 2) pos6 = pos6 + sign(pos5 - pos6)
    if (max(abs(pos7 - pos6)) >= 2) pos7 = pos7 + sign(pos6 - pos7)
    if (max(abs(pos8 - pos7)) >= 2) pos8 = pos8 + sign(pos7 - pos8)
    if (max(abs(tpos - pos8)) >= 2) tpos = tpos + sign(pos8 - tpos)
    
    tposframe = rbind(tposframe, tpos)
  }
}
nrow(unique(tposframe))