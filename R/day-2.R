# --- Day 2: 1202 Program Alarm ---

# Part 1
run_prog <- function(prog, r1, r2) {
  prog[2:3] <- c(r1, r2)
  for (i in seq(1, length(prog), 4)) {
    if(prog[i] == 99) {
      return(prog[1])
    } else {
      prog[prog[i + 3] + 1] <- do.call(c("sum", "prod")[prog[i]], list(prog[prog[i + 1:2] + 1]))
    }
  }
}

run_prog(scan("input/2.txt", sep = ","), 12, 2)

# Part 2
grid <- expand.grid(noun = 0:99, verb = 0:99)
sum(grid[which(mapply(run_prog, list(scan("input/2.txt", sep = ",")), grid$noun, grid$verb) == 19690720),] * c(100, 1))

# Bonus plot
library(tidyverse)
expand.grid(noun = 0:99, verb = 0:99) %>%
  mutate(return = mapply(run_prog, list(scan("input/2.txt", sep = ",")), noun, verb)) %>%
  mutate(return = abs(return-19690720)) %>%
  ggplot(aes(x=noun, y = verb, fill = return)) +
  geom_tile() +
  theme(legend.position = "bottom")