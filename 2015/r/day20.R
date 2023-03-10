# --- Day 20: Infinite Elves and Infinite Houses ---

find_lowest <- function(n) {
  
  # Find a reasonable maxn
  maxn <- n
  for (i in 50:5) {
    gifts <- sum(which(((n %/% i) %% seq_len(n %/% i)) == 0)) * 10
    if (gifts >= n) {
      maxn <- n %/% i
      break
    }
  }
  
  divisors = rep(0, maxn)

  for (i in seq(1, maxn)) {
    for (j in seq(i, maxn, i)) {
      divisors[j] <- divisors[j] + i * 10
      if (i == j && divisors[j] >= n) return(j)
    }
  }
}

# Part 1
find_lowest(331000)
