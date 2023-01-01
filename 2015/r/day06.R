# --- Day 6: Probably a Fire Hazard ---

instructions <- readLines('2015/data/input06.txt') |>
  strsplit(' |,|e')

# Part 1
part_1 <- function(instructions) {
  lights <- expand.grid(x = 0:999, y = 0:999, state = FALSE)

  for (i in instructions) {
    selected_lights <- (
      lights$x >= as.numeric(i[3]) &
        lights$x <= as.numeric(i[6]) &
        lights$y >= as.numeric(i[4]) &
        lights$y <= as.numeric(i[7])
    )
    
    if (i[2] == 'on') {
      lights$state[selected_lights] <- TRUE
    } else if (i[2] == 'off') {
      lights$state[selected_lights] <- FALSE
    } else {
      lights$state[selected_lights] <- !lights$state[selected_lights]
    }
  }
    return(sum(lights$state))
}

part_1(instructions)

# Part 2
part_2 <- function(instructions) {
  lights <- expand.grid(x = 0:999, y = 0:999, state = 0)
  
  for (i in instructions) {
    selected_lights <- (
      lights$x >= as.numeric(i[3]) &
        lights$x <= as.numeric(i[6]) &
        lights$y >= as.numeric(i[4]) &
        lights$y <= as.numeric(i[7])
    )
    
    if (i[2] == 'on') {
      lights$state[selected_lights] <- lights$state[selected_lights] + 1
    } else if (i[2] == 'off') {
      lights$state[selected_lights] <- pmax(lights$state[selected_lights] - 1, 0)
    } else {
      lights$state[selected_lights] <- lights$state[selected_lights] + 2
    }
  }
  return(sum(lights$state))
}

part_2(instructions)
