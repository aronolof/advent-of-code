# --- Day 13: Packet Scanners ---
input <- readLines('2017/data/input13.txt') |>
  strsplit(': ') |>
  lapply(as.numeric)

packet_scanners <- function(input) {
  input |>
    sapply(\(x) {
      if (((x[1]) %% ((x[2] - 1) * 2)) == 0) {
        return(prod(x))
      } else {
        return(0)
      }
    }) |>
    sum()
}

# Part 1
packet_scanners(input)

