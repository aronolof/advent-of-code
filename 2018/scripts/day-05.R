# --- Day 5: Alchemical Reduction ---

input <- readLines('2018/input/input-05')

# Part 1
# Loop version

v <- utf8ToInt(input)

for(i in 0:1000) {
  compare = v[pmin(pmax(seq(v) + sign((i+seq(v)) %% 2-.5), 1), max(seq(v)))]
  new_v <- v[abs(v - compare) != 32]
  if(identical(v, new_v)) break
  v <- new_v
}
nchar(intToUtf8(v))

# Recursive function version
(\(v, i = 0, no_change = F) {
  compare = v[pmin(pmax(seq(v) + sign((i+seq(v)) %% 2-.5), 1), max(seq(v)))]
  new_v <- v[abs(v - compare) != 32]
  if(no_change & identical(v, new_v)) return(new_v)
  Recall(new_v, i+1, identical(v, new_v))
})(utf8ToInt(input)) |>
  intToUtf8() |>
  nchar()


