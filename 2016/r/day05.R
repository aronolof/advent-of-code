# --- Day 5: How About a Nice Game of Chess? ---
library(digest)

input <- 'uqwqemis'

# Part 1
password <- c()
i <- 0
while (TRUE) {
  hash <- paste0(input, i) |>
    digest(algo = 'md5', serialize = FALSE)
  
  if (substr(hash, 1, 5) == '00000') {
    password <- c(password, substr(hash, 6, 6))
    print(password)
  }
  if (length(password) == 8) {
    break
  }
  if (i %% 100000 == 0) {
    print(i)
  }
  i <- i + 1
}
paste(password, collapse = '')

# Part 2
password <- rep(NA, 8)
i <- 0
while (TRUE) {
  hash <- paste0(input, i) |>
    digest(algo = 'md5', serialize = FALSE)
  
  if (substr(hash, 1, 5) == '00000') {
    pos <- substr(hash, 6, 6)
    
    if (pos %in% as.character(0:7)) {
      pos <- as.numeric(pos) + 1
      if (is.na(password[pos])) {
        password[pos] <- substr(hash, 7, 7)
      }
      print(password)
    }
  }
  if (sum(is.na(password)) == 0) {
    break
  }
  if (i %% 100000 == 0) {
    print(i)
  }
  i <- i + 1
}
paste(password, collapse = '')
