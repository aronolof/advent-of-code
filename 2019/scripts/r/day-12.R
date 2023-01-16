# --- Day 12: The N-Body Problem ---

input <- readLines('2019/input/input-12.txt') |>
  strsplit('[^0-9-]+') |>
  lapply(\(x) as.numeric(x[-1]))

# Part 1
position <- input
velocity <- lapply(1:4, \(x) rep(0, 3))

for (i in 1:1000) {
  velocity <- lapply(1:4,
         \(x) {
           forces <- sapply((1:4)[-x], \(y) {
             sign(position[[y]] - position[[x]])
           }) |>
             rowSums()
           
           velocity[[x]] + forces
         }
         )
  
  position <- lapply(1:4, \(x) {
    position[[x]] + velocity[[x]]
  })
}

(sapply(position, \(x) sum(abs(x))) * sapply(velocity, \(x) sum(abs(x)))) |>
  sum()
  
