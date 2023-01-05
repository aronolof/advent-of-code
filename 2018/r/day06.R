# --- Day 6: Chronal Coordinates ---

input <- read.csv('2018/data/input06.txt', header = F) 

# Part 1
space <- matrix(NA, nrow = max(input$V2), ncol = max(input$V1))

for (y in 1:nrow(space)) {
  for (x in 1:ncol(space)) {
    nearest <- rownames(input)[rank(abs(input$V1 - x) + abs(input$V2 - y)) == 1]
    space[y, x] <- ifelse(length(nearest) == 0, '0', nearest)
  }
}

infinite <- unique(c(0, space[c(1, nrow(space)),], space[,c(1, ncol(space))]))
areas <- sort(table(space), TRUE)
areas[setdiff(names(areas), infinite)][1]

# Part 2
space <- matrix(NA, nrow = max(input$V2), ncol = max(input$V1))

for (y in 1:nrow(space)) {
  for (x in 1:ncol(space)) {
    space[y, x] <- sum(abs(input$V1 - x) + abs(input$V2 - y)) < 10000
  }
}
sum(space)