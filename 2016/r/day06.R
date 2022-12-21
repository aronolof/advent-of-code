# --- Day 6: Signals and Noise ---
input <- readLines("2016/data/input06.txt")

decode <- function(input, part = 1) {
  input |>
    strsplit('') |>
    sapply(\(x) x) |>
    apply(1, \(x) {
      x |>
        table() |>
        sort() |>
        names() |>
        c(tail, head)[[part]](1)
    }) |>
    paste(collapse = '')
}

# Part 1
decode(input, part = 1)

# Part 2
decode(input, part = 2)
