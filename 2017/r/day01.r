input <- as.numeric(strsplit(readLines('2017/data/input01.txt'), '')[[1]])

# Part 1
sum(x[x == c(x[-1], x[1])])

# Part 2
shift <- 1 + (seq(x) + length(x)/2 - 1) %% length(x)
sum(x[x == x[shift]])

