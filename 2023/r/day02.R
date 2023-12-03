# --- Day 2: Cube Conundrum ---

input <- readLines("2023/data/input02.txt")

# Part 1
required_cubes <- c('blue' = 14, 'green' = 13, 'red' = 12)

game_max <- input |>
  strsplit('; ') |>
  sapply(\(game) {
    sapply(game, \(reveal) {
      parts <- strsplit(reveal, ', | ')[[1]]
      sapply(names(required_cubes), \(color) {
        n_cubes <- parts[which(parts == color) - 1]
        if(length(n_cubes)) return(as.numeric(n_cubes))
        return(0)
        })
      }) |>
      apply(1, max)
    })

game_max |>
  apply(2, \(x) all(x[names(required_cubes)] <= required_cubes)) |>
  which() |>
  sum()

# Part 2
game_max |>
  apply(2, prod) |>
  sum()
