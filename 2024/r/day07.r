# --- Day 7: Bridge Repair ---
input <- readLines('2024/data/input07.txt') |>
  strsplit(': | ') |>
  sapply(as.numeric)

# Part 1

check <- function(x) {
  if(x[1] > prod(x[-1])) return(0)
  if(x[1] == prod(x[-1])) return(x[1])
  if(x[1] == sum(x[-1])) return(x[1])
  
  gaps <- length(x) - 2
  n <- 2^gaps
  
  for(i in seq(n)) {
    total <- x[2]
    if(total > x[1]) break
    for(j in seq(gaps)) {
      
      if(intToBits(i)[j] == 1) {
        total <- total * x[2+j]
      } else {
        total <- total + x[2+j]
      }
    }
    if(total == x[1]) {
      return(sum(x[1]))
    }
  }
  return(0)
}

options(scipen = 999)
sum(sapply(input, check))

# Part 2

check <- function(x) {
  if(x[1] > as.numeric(paste0(x[-1], collapse = ''))) return(0)
  if(x[1] == prod(x[-1])) return(x[1])
  if(x[1] == sum(x[-1])) return(x[1])
  
  gaps <- length(x) - 2
  n <- 3^gaps
  
  for(i in seq(n)) {
    total <- x[2]
    
    for(j in seq(gaps)) {
      if(total > x[1]) break
      if(((i%/%(3^(j-1))) %% 3) == 0) {
        total <- total * x[2+j]
      } else if (((i%/%(3^(j-1))) %% 3) == 1)  {
        total <- total + x[2+j]
      } else {
        total <- as.numeric(paste0(total, x[2+j]))
      }
    }
    if(total == x[1]) {
      return(sum(x[1]))
    }
  }
  return(0)
}

ans <- 0
for(i in seq(input)) {
  ans <- ans + check(input[[i]])
  #print(paste(i, ': ', ans))
}
ans
