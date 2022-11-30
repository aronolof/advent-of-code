input <- readLines('2017/data/input01')

# Part 1
x <- as.numeric(strsplit(input, '')[[1]])
sum(x[x == c(x[-1], x[1])])