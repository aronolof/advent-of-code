# --- Day 4: Camp Cleanup ---
x <- read.table(text = gsub("-|,", " ", readLines("2022/data/input04.txt")))

# Part 1
sum((x[1] >= x[3] & x[2] <= x[4]) | (x[1] <= x[3] & x[2] >= x[4]))

# Part 2
sum((x[2] >= x[3] & x[1] <= x[4]))
