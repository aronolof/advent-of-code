# --- Day 4: Giant Squid ---

draws <- scan('2021/input/input-04.txt', nlines=1, sep = ',')
boards <- split(scan('2021/input/input-04.txt', skip=1), rep(1:100, each=25))

# Part 1 and 2
results <- sapply(boards,
                  \(x) min(sapply(1:2, \(y) apply(matrix(match(x, draws), 5), y, max))))

sapply(c(which.min(results), which.max(results)),
       \(x) sum(boards[[x]][!(boards[[x]] %in% draws[seq(results[x])])]) * draws[results[x]])
