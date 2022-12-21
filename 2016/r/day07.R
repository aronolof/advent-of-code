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
        z[i] == z[i-3] & z[i-1] == z[i-2] & z[i] != z[i-1]
      }) |>
        any()
    })
    
    any(abba[c(T, F)]) & !any(abba[c(F, T)])
  }) |>
  sum()
