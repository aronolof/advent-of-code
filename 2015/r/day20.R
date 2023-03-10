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
  
  best_answer <- Inf
  #maxn <- min(2, n %/% 10)
  divisors = rep(0, maxn)
  
  for (i in seq(1, maxn)) {
    for (j in seq(i, maxn, i)) {
      divisors[j] <- divisors[j] + i * 10
      
      if (j < best_answer && divisors[j] >= n) {
        best_answer <- j
        #print(best_answer)
        if (i == j) return(best_answer)
      }
    }
  }
  return(which(divisors >= n)[1])
}

# Part 1
find_lowest(33100000)
