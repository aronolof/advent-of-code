# --- Day 5: Alchemical Reduction ---

input <- readLines('2018/input/input-05')

# Part 1
reaction <- function(input) {
  v <- utf8ToInt(input)
  i <- 0
  while(TRUE) {
    compare = v[pmin(pmax(seq(v) + sign((i+seq(v)) %% 2-.5), 1), max(seq(v)))]
    new_v <- v[abs(v - compare) != 32]
    if(identical(v, new_v)) return(nchar(intToUtf8(v)))
    v <- new_v
    i <- i + 1
  }
}
reaction(input)

# Part 2 apply
sapply(1:26, \(x) {
  reaction(gsub(paste(letters[x], LETTERS[x], sep = '|'), '', input))
  }) |>
  min()

# Part 2 loop
shortest <- c()
for(x in 1:26) {
  shortest[x] <- reaction(gsub(paste(letters[x], LETTERS[x], sep = '|'), '', input))
  print(shortest[x])
}
min(shortest)
