# --- Day 5: How About a Nice Game of Chess? ---
library(digest)

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


