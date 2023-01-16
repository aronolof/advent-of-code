# --- Day 13: A Maze of Twisty Little Cubicles ---

cubicle_maze <- function(input, part = 1) {
  
  is_wall <- function(pos, input) {
    x <- pos[1]
    y <- pos[2]
    z <- (x^2 + 3*x + 2*x*y + y + y^2) + input
    z <- sum(intToBits(z) == '01') %% 2
    as.logical(z)
  }
  
  start_pos <- c(1, 1, 0)
  target_pos <- c(31, 39)
  
  visited = list()
  queue = list(start_pos)
  input = 1358
  
  while (TRUE) {
    pos <- queue[1][[1]]
    
    if (part == 1 && all(pos[1:2] == target_pos)) {
      return(pos[3])
    } else if (part == 2 && pos[3] > 50) {
      return(length(visited))
    }
    
    queue[1] = NULL
    visited <- append(visited, list(pos))
    
    lapply(list(c(0, 1), c(0, -1), c(1, 0), c(-1, 0)),
           \(n) {
             conditions <- all(
               min(pos[1:2] + n) >= 0,
               !is_wall(c(pos[1:2] + n), input),
               !(list(pos[1:2] + n) %in% lapply(visited, \(x) x[1:2])),
               !(list(pos[1:2] + n) %in% lapply(queue, \(x) x[1:2]))
             )
             if (conditions) {
               queue <<- append(queue, list(c(pos[1:2] + n, pos[3] + 1)))
             }
           })
  }
}

cubicle_maze(input = 1358, part = 1)
cubicle_maze(input = 1358, part = 2)

