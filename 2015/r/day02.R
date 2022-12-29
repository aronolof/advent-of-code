# --- Day 2: I Was Told There Would Be No Math ---
input <- read.table('2015/data/input02.txt', sep = 'x')

# Part 1
apply(input, 1, \(x) {
  sides <- c(prod(x[1:2]), prod(x[2:3]), prod(x[c(1,3)]))
  sum(2 * sides) + min(sides)
}) |>
  sum()

# Part 2
apply(input, 1, \(x) sum(head(sort(x), 2)) * 2 + prod(x)) |>
  sum()
