# --- Day 7: Camel Cards ---

input <- read.delim("2023/data/input07.txt", header = FALSE, sep = ' ')

# Part 1
cards <- setNames(14:2, c('A', 'K', 'Q', 'J', 'T', 9:2))

res <- apply(input, 1, \(x) {
    hand <- strsplit(x[1], '')[[1]]
    
    strength <- sort(table(hand), decreasing = TRUE)[1:2] |>
      paste(collapse='') |>
      match(c('11', '21', '22', '31', '32', '41', '5'))
    
    c(strength, cards[hand], as.numeric(x[2]))
  })

sum(res[7, order(res[1,], res[2,], res[3,], res[4,], res[5,], res[6,])] * seq(ncol(res)))

# Part 2
res <- apply(input, 1, \(x) {
    hand <- strsplit(x[1], '')[[1]]
    
    if (sum(hand == 'J') %in% c(0, 5)) {
      max_strength <- strength(hand)
    } else {
      max_strength <- replicate(sum(hand == 'J'), unique(hand[hand != 'J']), simplify = FALSE) |>
        expand.grid() |>
        merge(matrix(hand[hand != 'J'], nrow = 1)) |>
        apply(1, \(hand) {
          sort(table(hand), decreasing = TRUE)[1:2] |>
            paste(collapse='') |>
            match(c('11', '21', '22', '31', '32', '41', '5'))
        }) |>
        max()
    }
    
    c(max_strength, c(cards[-4], c('J' = 1))[hand], as.numeric(x[2]))
  })

sum(res[7, order(res[1,], res[2,], res[3,], res[4,], res[5,], res[6,])] * seq(ncol(res)))
