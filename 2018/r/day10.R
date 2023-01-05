# --- Day 10: The Stars Align ---
instructions <- readLines('2018/data/input10.txt') |>
  strsplit('[^0-9-]+', ',') |>
  sapply(\(x) as.numeric(tail(x, -1))) |>
  t()

best_position <- instructions[, 1:2]
best_prop_aligned <- mean(duplicated(best_position[,1]) & best_position[,2])

for (i in 1:20000) {
  current_position <- instructions[, 1:2] + i * instructions[, 3:4]
  prop_aligned <- mean(duplicated(current_position[,1]) & current_position[,2])
  
  if (prop_aligned > best_prop_aligned) {
    best_position <- current_position
    best_prop_aligned <- prop_aligned
    best_i <- i
  }
}

# Part 1
best_position |>
  apply(1, \(x) x * c(1, -1)) |>
  t() |>
  plot(axes = FALSE, pch = 15)

# Part 2
best_i
