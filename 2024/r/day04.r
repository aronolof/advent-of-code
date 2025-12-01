#--- Day 3: Mull It Over ---
input <- readLines('2024/data/test04.txt') |>
  strsplit('')

m <- do.call(rbind, input)

search = 'XMAS'

# apply 1
step1 <- apply(m, 1, \(x) {
  x1 <- paste(x, collapse='')
  x2 <- paste(rev(x), collapse='')
  
  length(regmatches(x1, gregexpr(search, x1))[[1]]) + 
    length(regmatches(x2, gregexpr(search, x2))[[1]])
})

# apply 2
step2 <- apply(m, 2, \(x) {
  x1 <- paste(x, collapse='')
  x2 <- paste(rev(x), collapse='')
  
  length(regmatches(x1, gregexpr(search, x1))[[1]]) + 
    length(regmatches(x2, gregexpr(search, x2))[[1]])
})

# diag 1
#from -nrow to + nrow
nrow(m)

col(m) == row(m) 


# diag 2
m[col(m) + row(m) == 11]

col(m) + row(m) == 20
