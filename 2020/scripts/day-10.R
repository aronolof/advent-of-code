# --- Day 10: Adapter Array ---
  
input <- scan("2020/input/input-10.txt")

# Part 1
prod(table(diff(c(0, sort(input), max(input) + 3))))

# Part 2
adapter_diff_lengths <- data.frame(unclass(rle(diff(c(0, sort(input), max(input) + 3)))))

format(prod(apply(adapter_diff_lengths, 1, function (x) {
  if (x[2] == 1 & x[1] == 2) {2}
  else if (x[2] == 1 & x[1] == 3) {4}
  else if (x[2] == 1 & x[1] == 4) {7}
  else {1}
})), digits = 10)