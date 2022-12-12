# --- Day 11: Monkey in the Middle ---
library(igraph)

m <- readLines("2022/data/input12.txt") |>
  strsplit('') |>
  sapply(match, c('S', letters, "E"))

edgelist = lapply(seq(m), \(s) {
  i <- row(m)[s]
  j <- col(m)[s]
  height <- m[i, j]
  walkable <- which(abs(row(m) - i) + abs(col(m) - j) == 1 & m - height <= 1)
  expand.grid(s, walkable)
  })

g <- do.call(rbind, edgelist) |>
  as.matrix() |>
  graph_from_edgelist()

# Part 1
shortest_paths(g, from = which.min(m), to = which.max(m))$vpath[[1]] |>
  tail(-1) |>
  length()

# Part 2
sapply(which(m <= 2), \(i) {
  steps <- shortest_paths(g, from = i, to = which.max(m))$vpath[[1]] |>
    suppressWarnings() |>
    tail(-1) |>
    length()
  ifelse(steps == 0, Inf, steps)
}) |>
  min()


# Plot
library(ggplot2)
shortest_path <- shortest_paths(g, from = which.min(m), to = which.max(m))$vpath[[1]]

reshape2::melt(m) |>
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_path(data = reshape2::melt(m)[shortest_path,], colour = 'yellow') +
  coord_equal()  +
  theme_void() +
  theme(legend.position = 'none')
  
