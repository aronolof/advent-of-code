# --- Day 13: Transparent Origami ---

input <- readLines("2021/input/input-13.txt")

fold <- read.table(text = gsub("fold along ", "", input[grepl("^f", input)]), sep = "=")
dots <- read.table(text = input[grepl("^\\d", input)], sep = ",", col.names = c("x", "y"))

for (i in 1:nrow(fold)) {
  dots[fold[i, 1]] <- pmin(2 * fold[i, 2] - dots[fold[i, 1]], dots[fold[i, 1]])
  if (i == 1) print(nrow(unique(dots))) # Part 1
}

plot(dots, ylim = rev(range(dots$x))) # Part 2
