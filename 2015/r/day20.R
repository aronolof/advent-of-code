# --- Day 20: Infinite Elves and Infinite Houses ---

find_lowest <- function(n, multiplier = 10, max_n_presents = Inf) {
  
  max_n <- max(1, n %/% multiplier)
  divisors = rep(0, max_n)

  for (i in seq(1, max_n)) {
    for (j in head(seq(i, max_n, i), max_n_presents)) {
      divisors[j] <- divisors[j] + i * multiplier
      
      if (i == j && divisors[j] >= n) {
        return(j)
      }
    }
  }
}

# Part 1
find_lowest(33100000, multiplier = 10)

# Part 2
find_lowest(33100000, multiplier = 11, max_n_presents = 50)
