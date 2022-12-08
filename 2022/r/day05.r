# --- Day 5: Supply Stacks ---
input <- readLines("2022/data/input05.txt")

arrange_crates <- function(input, CrateMover = 9000) {
  procedure <- input[grep("m", input)] |>
    strsplit(" ") |>
    lapply(\(x) as.integer(x[c(2, 4, 6)]))

  crates <- input[grep("\\[", input)] |>
    strsplit("m") |>
    sapply(\(x) x[seq(2, length(x), 4)]) |>
    apply(1, \(x) rev(x[x != " "]))

  for (p in procedure) {
    move <- p[1]
    from <- p[2]
    to <- p[3]

    moved_crates <- ifelse(CrateMover >= 9001, `c`, `rev`)(tail(crates[[from]], move))

    crates[[to]] <- c(crates[[to]], moved_crates)
    crates[[from]] <- head(crates[[from]], -move)
  }

  sapply(crates, tail, 1) |>
    paste(collapse = "")
}

# Part 1
arrange_crates(input)
# Part 2
arrange_crates(input, CrateMover = 9001)
