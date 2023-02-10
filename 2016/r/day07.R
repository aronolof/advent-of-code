# --- Day 7: Internet Protocol Version 7 ---
input <- readLines("2016/data/input07.txt")

# Part 1
input |>
  gsub(']', '[', x = _) |>
  strsplit('\\[') |>
  sapply(\(x) {
    y <- strsplit(x, split = '')
    
    abba <- sapply(y, \(z) {
      sapply(seq(4, length(z)), \(i){
        z[i] == z[i - 3] & z[i - 1] == z[i - 2] & z[i] != z[i - 1]
      }) |>
        any()
    })
    
    any(abba[c(T, F)]) & !any(abba[c(F, T)])
  }) |>
  sum()

# Part 2
input |>
  strsplit('\\[|\\]') |>
  sapply(\(x) {
    
    bab <- x[(seq(x) %% 2) == 0] |>
             paste0(collapse = '|')
    
    aba <- x[(seq(x) %% 2) == 1] |>
      paste('') |>
      strsplit('') |>
      unlist()
    
    for (i in seq(3, length(aba))) {
      if (aba[i - 2] == aba[i] && aba[i - 1] != aba[i]) {
        if (grepl(paste(aba[c(i - 1, i, i - 1)], collapse = ''), bab)) {
          return(TRUE)
        }
      } 
    }
    return(FALSE)
  }) |>
  sum()
