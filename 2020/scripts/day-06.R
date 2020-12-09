# --- Day 6: Custom Customs ---

input <- (function(x) split(x, cumsum(x == "")))(readLines("2020/input/input-06.txt"))

# Part 1
sum(sapply(input, function(x) length(unique(unlist(strsplit(x, ""))))))

# Part 2
sum(sapply(input, function(x) length(Reduce(intersect, strsplit(x[x != ""], "")))))
