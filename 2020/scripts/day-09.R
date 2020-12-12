# --- Day 9: Encoding Error ---

input <- as.numeric(readLines("2020/input/input-09.txt"))

# Part 1
for(i in 26:length(input)) {
  if(!(input[i] %in% rowSums(expand.grid(input[seq(i-25, i-1)], input[seq(i-25, i-1)])))){
    ans_1 <- input[i]
  }
}
ans_1

# Part 2
tbl <- (function(x) x[x[[1]] < x[[2]],])(expand.grid(seq(input), seq(input)))

for(i in seq(nrow(tbl))) {
  range_sum <- sum(input[tbl[i,1]:tbl[i,2]])
    
  if(range_sum == ans_1) {
    ans_2 <- sum(range(input[tbl[i,1]:tbl[i,2]]))
    break
    }
}
ans_2
