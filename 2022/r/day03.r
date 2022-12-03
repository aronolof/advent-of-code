# --- Day 3: Rucksack Reorganization ---
items <- lapply(readLines("2022/data/input03.txt"), \(x) (utf8ToInt(x)-38) %% 58)

# Part 1
sum(sapply(items, \(x) Reduce(intersect, split(x, seq(x) <= length(x)/2))))

# Part 2
sum(sapply(split(items, (seq(length(items)) - 1) %/% 3), \(x) Reduce(intersect, x)))
