# --- Day 1: No Time for a Taxicab ---
x <- scan("2016/data/input01.txt", what = "") |>
  gsub('R|,', '', x = _) |>
  gsub('L', '-', x = _) |>
  as.numeric()

# Part 1
dirs <- sign((cumsum(sign(x)) %% 4) - 1.5)
abs(sum((abs(x) * dirs)[c(T, F)])) + abs(sum((abs(x) * dirs)[c(F, T)]))

# Part 2
lr <- cumsum(rep(sign(abs(x) * dirs * c(T, F)), abs(x)))
ud <- cumsum(rep(sign(abs(x) * dirs * c(F, T)), abs(x)))
combined <- complex(real = lr, imaginary = ud)

loc <- sapply(tail(seq(combined), -1), \(i) combined[i] %in% combined[seq(i - 1)])
abs(lr[which(loc)[1] + 1]) + abs(ud[which(loc)[1] + 1])
