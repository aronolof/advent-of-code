# --- Day 3: Binary Diagnostic ---

input <- scan('2021/input/input-03.txt', what = 'character')

# Part 1
sapply(1:12, \(i) names(sort(table(sapply(strsplit(input, ''), "[[", i)))[2])) |>
  (\(x) prod(strtoi(paste(x, collapse = ''), 2), strtoi(paste(as.numeric(x == '0'), collapse = ''), 2)))()

# Part 2
oxy <- matrix(unlist(strsplit(input, '')), ncol = 12, byrow = TRUE)
for(i in 1:12) {
  most_common <- names(sort(table(oxy[,i]) + c(0, .5)))[2]
  oxy <- oxy[oxy[,i] == most_common,,drop=FALSE]
  if(nrow(oxy) == 1) break
}

co2 <- matrix(unlist(strsplit(input, '')), ncol = 12, byrow = TRUE)
for(i in 1:12) {
  most_common <- names(sort(table(co2[,i]) + c(0, .5)))[1]
  co2 <- co2[co2[,i] == most_common,,drop=FALSE]
  if(nrow(co2) == 1) break
}

prod(strtoi(paste(oxy, collapse = ""), 2), strtoi(paste(co2, collapse = ""), 2))
