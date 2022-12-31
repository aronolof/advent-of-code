# --- Day 4: The Ideal Stocking Stuffer ---

# install.packages('digest')
library(digest)

find_hash <- function(input, n_zeros = 5, chunk_size = 10000) {
  j <- 0
  hashes <- numeric(chunk_size)
  zeros <- paste(rep(0, n_zeros), collapse = '')
  while (TRUE) {
    print(paste('Searched:', j))
    hashes[seq(hashes)] <- sapply(seq(j, j + chunk_size - 1), \(i) {
      paste0(input, i) |>
        digest(algo = 'md5', serialize = FALSE) |>
        substr(1, n_zeros)
    })
    
    if (any(hashes == zeros)) {
      return(paste('Answer:', j + which(hashes == zeros) - 1)[1])
      break
    } else {
      j <- j + chunk_size
    }
  }
}

# Part 1
find_hash('yzbqklnj', n_zeros = 5)

# Part 2
find_hash('yzbqklnj', n_zeros = 6)
