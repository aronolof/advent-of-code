input <- readLines("2022/data/test14.txt") |>
  strsplit('\\D+') |>
  sapply(as.numeric)

draw_cave <- function(input, part = 1) {
  max_y <- max(unlist(input)[c(F, T)])
  
  cave <- matrix(NA, nrow = max_y + 2, ncol = 500 + max_y + 4)
  
  for (path in input) {
    for (i in seq(1, length(path) - 3, 2)) {
      cave[path[i + 1]:path[i + 3], path[i]:path[i + 2]] <- '#'
    }
  }
  
  if (part == 2) {
    cave[max_y + 2, (-max_y - 3):(max_y + 3) + 500] <- '#'
  }
  return(cave)
}

simulate_sand <- function(cave) {
  while (TRUE) {
    
    if (sum(is.na(cave[1, 499:501])) == 0) {
      return(sum(cave == 'o', na.rm = T) + 1)
    }
    
    sand_pos <- c(0, 500)
    while (TRUE) {
      if (sand_pos[1] >= nrow(cave)) {
        return(sum(cave == 'o', na.rm = T))
      }
      
      check_below <- cave[sand_pos[1] + 1, sand_pos[2] + -1:1] |>
        is.na()
      
      if (check_below[2]) {
        sand_pos <- sand_pos + c(1, 0)
      } else if (check_below[1]) {
        sand_pos <- sand_pos + c(1, -1)
      } else if (check_below[3]) {
        sand_pos <- sand_pos + c(1, 1)
      } else {
        cave[sand_pos[1], sand_pos[2]] <- 'o'
        break
      }
    }
  }
}

# Part 1
draw_cave(input) |>
  simulate_sand()

# Part 2
draw_cave(input, part = 2) |>
  simulate_sand()