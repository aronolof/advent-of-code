# --- Day 17: Trick Shot ---

input <- readLines("2021/input/input-17.txt")

# Part 1
area <- strsplit(gsub("^.*x=([-0-9]*)\\.\\.([-0-9]*), y=([-0-9]*)\\.\\.([-0-9]*)", "\\1 \\2 \\3 \\4", input), " ")[[1]] |>
  as.numeric()

(abs(area[3]) - (area[3] < 0)) |>
  (\(x) (x^2 + x) / 2)()

# Part 2
in_target_area <- function(x, y) {
  velocity <- c(x, y)
  i <- 1
  pos <- c(x = 0, y = 0)
  repeat({
    pos <- pos + velocity

    velocity[1] <- velocity[1] - sign(velocity[1])
    velocity[2] <- velocity[2] - 1

    if (pos[1] >= area[1] & pos[1] <= area[2] & pos[2] >= area[3] & pos[2] <= area[4]) {
      return(TRUE)
    } else if (pos[2] < area[3] | pos[1] > area[2]) {
      return(FALSE)
    } else {
      i <- i + 1
    }
  })
}

lower_x <- ceiling(sqrt(2 * area[1] + .25) - .5)
# upper_x <- floor(sqrt(2*area[2] + .25) - .5)

expand.grid(lower_x:area[2], area[3]:(abs(area[3]) - (area[3] < 0))) |>
  apply(1, \(x) in_target_area(x[1], x[2])) |>
  sum()
