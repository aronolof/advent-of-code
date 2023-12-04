# --- Day 4: Scratchcards ---

input <- readLines("2023/data/input04.txt")

# Part 1
wins <- strsplit(input, ' \\| ') |>
  sapply(\(x) {
    card <- strsplit(x, ' +')
    sum(card[[1]] %in% card[[2]])
  })
sum((wins >= 1) * (2 ^ (wins - 1)))

# Part 2
n_cards <- wins ^ 0
for (i in seq(n_cards)) {
  if(wins[i] > 0) {
    n_cards[i + seq(wins[i])] <- n_cards[i + seq(wins[i])] + n_cards[i]
  }
}
sum(n_cards)
