# --- Day 5: Alchemical Reduction ---

input <- readLines('2018/input/input-05')

# Part 1 
reaction <- \(v, i = 0, no_change = F) {
  compare = v[pmin(pmax(seq(v) + sign((i+seq(v)) %% 2-.5), 1), max(seq(v)))]
  new_v <- v[abs(v - compare) != 32]
  if(no_change & identical(v, new_v)) return(new_v)
  Recall(new_v, i+1, identical(v, new_v))
}

nchar(intToUtf8(reaction(utf8ToInt(input))))

# Part 2
shortest = nchar(intToUtf8(reaction(utf8ToInt(input))))
for(x in 1:26) {
  removed <- gsub(paste(letters[x], LETTERS[x], sep = '|'), '', input)
  shortest <- min(
    shortest,
    nchar(intToUtf8(reaction(utf8ToInt(removed))))
  )
}
shortest

sapply(1:26, \(x) {
  utf8ToInt(gsub(paste(letters[x], LETTERS[x], sep = '|'), '', input)) |>
    reaction() |>
    intToUtf8() |>
    nchar()
})

