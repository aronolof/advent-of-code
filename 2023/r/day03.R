# --- Day 3: Gear Ratios ---

input <- readLines("2023/data/input03.txt")

m <- strsplit(input, "(?=[^0-9])", perl = TRUE)

total_sum <- 0

for(i in seq(input)) {
  mi <- m[[i]]
  l <- c(1, cumsum(nchar(mi)))
  r <- cumsum(nchar(mi)) + 1
  
  for (j in grep('[0-9]', mi)) {
      s <- c(
        substr(input[i-1], l[j], r[j]),
        substr(input[i+1], l[j], r[j]),
        mi[c(j-1, j+1)]
        )
      
      if(any(grepl('[^0-9\\.]', s))) {
        total_sum <- total_sum + as.numeric(mi[j])
      }
  }
}
print(total_sum)

total_mult <- 0

for(i in seq(input)) {
  mi <- m[[i]]
    for(j in which(mi == '*')) {
    
    a <- cumsum(nchar(m[[i-1]]))
    l <- which(a >= (j-1))[1]
    r <- which(a >= (j+1))[1]
    
    a2 <- cumsum(nchar(m[[i+1]]))
    l2 <- which(a2 >= (j-1))[1]
    r2 <- which(a2 >= (j+1))[1]
    
    
    neighbors <- c(m[[i-1]][l:r],
    m[[i+1]][l2:r2],
    mi[c(j-1, j+1)])
    neighbors <- neighbors[grepl('[0-9]', neighbors)]
    if(length(neighbors) == 2) {
      total_mult <- total_mult + prod(as.numeric(neighbors))
    }
  }
}
total_mult


input <- readLines("2023/data/test03.txt")
engine <- do.call(rbind, strsplit(input, ''))

engine[10, 10] <- '*'

gears <- which(engine == '*', arr.ind = TRUE)

adjacent_parts <- apply(gears, 1, \(x) {
  lr <- max(0, x[1] - 1):min(ncol(engine), x[1] + 1)
  ud <- max(0, x[1] - 1):min(nrow(engine), x[1] + 1)
    
  m <- engine[lr, ud]
  apply(m, 1, \(y) {
    if(length(y) < 3) return(max(grepl('[0-9]', y)))
    if (all(grepl('[0-9]', y) == c(1, 0, 1))) return(2)
    if (sum(grepl('[0-9]', y)) == 0) return(0)
    return(1)
  }) |>
    sum()
})

active_gears <- gears[adjacent_parts == 2,]

part_numbers <- strsplit(input, "(?=[^0-9])", perl = TRUE)

for(i in seq(nrow(active_gears))) {
  
  
  rc <- active_gears[i,]
  active_gears
  substr(input[rc['row']], 0, )
  
  a <- part_numbers[[rc['row']]]
  a[cumsum(nchar(a)) == rc['col'] - 1]
  a[cumsum(nchar(a)) == rc['col'] + 1]
  
  b <- part_numbers[[rc['row'] + 1]]
  cumsum(nchar(b))
  
  cumsum(nchar(b))
  a[cumsum(nchar(a)) == rc['col'] - 1]
  a[cumsum(nchar(a)) == rc['col'] + 1]
  
  
  
  engine[2, 4]
  
}



