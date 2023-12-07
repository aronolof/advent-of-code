# --- Day 7: Camel Cards ---

input <- read.delim("2023/data/input07.txt", header = FALSE, sep = ' ')
cards <- setNames(14:2, c('A', 'K', 'Q', 'J', 'T', 9:2))

strength <- function(hand) {
  count <- sort(table(hand), decreasing = TRUE)
  
  if (count[1] == 5) 7
  else if (count[1] == 4) 6
  else if (all(count[1:2] == 3:2)) 5
  else if (count[1] == 3) 4
  else if (all(count[1:2] == 2:2)) 3
  else if (count[1] == 2) 2
  else 1
}

# Part 1
res <- input |>
  apply(1, \(x) {
    hand <- strsplit(x[1], '')[[1]]
    c(strength(hand), cards[hand], as.numeric(x[2]))
  })
sum(res[7, order(res[1,], res[2,], res[3,], res[4,], res[5,], res[6,])] * seq(ncol(res)))

# Part 2
recursive_find <- function(x) {
  if(all(grepl('J', x))) return(strength(rep('T', 5)))
  if(!any(grepl('J', x))) return(strength(x))
  
  h2 <- which(x == 'J')[1]
  
  testvals <- unique(x[x != 'J'])
  sapply(seq(testvals), \(j) {
    x2 <- x
    x2[which(x2 == 'J')[1]] <- testvals[j]
    x2
    recursive_find(x2)
  }) |>
    max()
}

res <- input |>
  apply(1, \(x) {
    hand <- strsplit(x[1], '')[[1]]
    c(recursive_find(hand), c(cards[-4], c('J' = 1))[hand], as.numeric(x[2]))
  })
sum(res[7, order(res[1,], res[2,], res[3,], res[4,], res[5,], res[6,])] * seq(ncol(res)))

# Alternative:
res <- input |>
  apply(1, \(x) {
    hand <- strsplit(x[1], '')[[1]]
    
    if (all(grepl('J', hand))) {
      max_strength <- strength(rep('T', 5))
    } else if (!any(grepl('J', hand))) {
      max_strength <- strength(hand)
    } else {
      max_strength <- replicate(sum(hand == 'J'), unique(hand[hand != 'J']), simplify = FALSE) |>
        expand.grid() |>
        merge(matrix(hand[hand != 'J'], nrow = 1)) |>
        apply(1, strength) |>
        max()
    }
    
    c(max_strength, c(cards[-4], c('J' = 1))[hand], as.numeric(x[2]))
  })
sum(res[7, order(res[1,], res[2,], res[3,], res[4,], res[5,], res[6,])] * seq(ncol(res)))





