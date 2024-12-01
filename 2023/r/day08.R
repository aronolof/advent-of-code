# --- Day 8: Haunted Wasteland ---

input <- readLines("2023/data/input08.txt")

t <- Sys.time()
# Part 1
network <- sapply(c(1, 8, 13), \(i) substr(input[-1:-2], i, i + 2))
path <- (strsplit(input[1], '')[[1]] == 'R') + 2

n_steps <- function(start_node, end_node) {
  i <- 1
  node <- start_node
  repeat {
    node <- network[network[, 1] == node, path[((i - 1) %% length(path)) + 1]]
    if (grepl(end_node, node)) return(i)
    i <- i + 1
  }
}
n_steps('AAA', 'ZZZ')

# Part 2
gcd <- \(a, b) ifelse(b == 0, a, gcd(b, a %% b))
lcm <- \(a, b) a * b / gcd(a, b)

mapply(n_steps,
       network[grepl('..A', network[, 1]), 1],
       '..Z') |>
  Reduce(lcm, x = _) |>
  format(scientific = FALSE)

t - Sys.time()