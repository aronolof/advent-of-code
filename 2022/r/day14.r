input <- readLines("2022/data/test14.txt") |>
  strsplit('\\D+') |>
  sapply(as.numeric)

draw_cave <- function(x, part = 1) {
  all_values <- input |>
    unlist() 
  
  y_range <- all_values[seq(2, length(all_values), 2)] |>
    range()
  
  #max_y <- max(unlist(input)[c(F, T)])
  
  paths <- input
  
  m <- matrix(NA, nrow = max(y_range) + 2, ncol = 500 + max(y_range) + 4)
  
  for (path in paths) {
    for (i in seq(1, (length(path) - 3), 2)) {
      m[path[i + 1]:path[i + 3], path[i]:path[i + 2]] <- '#'
    }
  }
  
  if (part == 2) {
    floor_y <- max(y_range) + 2
    floor_x <- (-floor_y - 1 + 500):(floor_y + 1 + 500)
    m[floor_y, floor_x] <- '#'
  }
  return(m)
}

simulate_sand <- function(m) {
  
  m_init <- m[,]
  m_state <- m_init
  sand_source <- 500
  
  stop = FALSE
  while (TRUE) {
    if (stop) break
    
    if(sum(is.na(m_state[1, (sand_source - 1):(sand_source + 1)])) == 0) {
      return(sum(m_state == 'o', na.rm = T) + 1)
    }
    
    s <- c(0, sand_source)
    while (TRUE) {
      if (s[1] >= nrow(m_state)) {
        return(sum(m_state == 'o', na.rm = T))
      }
      
      check_below <- m_state[s[1] + 1, (s[2] - 1):(s[2] + 1)] |>
        is.na()
      
      if (check_below[2]) {
        s <- s + c(1, 0)
      } else if (check_below[1]) {
        s <- s + c(1, -1)
      } else if (check_below[3]) {
        s <- s + c(1, 1)
      } else {
        m_state[s[1], s[2]] <- 'o'
        break
      }
    }
  }
}

# Simulate sand
draw_cave(input) |>
  simulate_sand()

draw_cave(input, part = 2) |>
  simulate_sand()