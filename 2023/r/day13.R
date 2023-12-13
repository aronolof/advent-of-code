# --- Day 12: Hot Springs ---
input <- readLines("2023/data/input13.txt")

# Part 1
groups <- cumsum(input == '')
sep <- which(input == '')
patterns <- split(input[-sep], groups[-sep]) |>
  lapply(\(x) do.call(rbind, strsplit(x, '')))

find_reflection <- function(m, n_smudges = 0) {
  h_lines <- which(rowSums(head(m, -1) != tail(m, -1)) <= n_smudges)
  v_lines <- which(rowSums(head(t(m), -1) != tail(t(m), -1)) <= n_smudges)
  
  check_lines <- \(x, m, mult = 1) {
    side1 <- rev(seq(1, x))[seq(min(i, abs(nrow(m) - x)))]
    side2 <- seq(i + 1, nrow(m))[seq(min(x, abs(nrow(m) - x)))]
    
    if (sum(m[side1,] != m[side2,]) == n_smudges) {
      return(x * mult)
    }
  }

  c(sapply(h_lines, check_lines, m = m, mult = 100), 
    sapply(v_lines, check_lines, m = t(m))) |>
    unlist()
}

sapply(patterns, find_reflection) |> sum()

# Part 2
sapply(patterns, find_reflection, n_smudges = 1) |> sum()
