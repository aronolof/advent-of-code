# --- Day 8: Treetop Tree House ---
input <- readLines("2022/data/input09.txt")
steps = strsplit(input, ' ')

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

## Part 1
# todo
