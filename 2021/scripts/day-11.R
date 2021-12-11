# --- Day 11: Dumbo Octopus ---

input <- apply(Reduce(rbind, strsplit(readLines('2021/input/input-11.txt'), '')), 2, as.numeric)

# Part 1
next_step <- function(state) {
  octopi <- (state$octopi + 1)%%10
  
  while(sum(octopi == 0) != 0) {
    padded_octopi <- cbind(10, rbind(10, octopi, 10), 10)
    
    neighbor_flash <- mapply(\(i, j) padded_octopi[(2:11) + i, (2:11) + j] == 0,
                             i = rep(-1:1, 3)[-5],
                             j = rep(-1:1, each = 3)[-5],
                             SIMPLIFY = FALSE
    )
    
    octopi[octopi == 0] <- octopi[octopi == 0] - 1
    octopi[octopi > 0] <- pmin((octopi[octopi > 0] + Reduce(`+`, neighbor_flash)[octopi > 0]), 10) %% 10
  }
  octopi[octopi < 0] <- 0
  
  list(octopi = octopi,
       flashes = state$flashes + sum(octopi == 0),
       current_flashes = sum(octopi == 0),
       steps = state$steps + 1)
}

state <- list(octopi = input,
                      flashes = 0,
                      current_flashes = 0,
                      steps = 0)

for(i in 1:100) state <- next_step(state)
state$flashes

# Part 2
while(state$current_flashes != 100) state <- next_step(state)
state$step