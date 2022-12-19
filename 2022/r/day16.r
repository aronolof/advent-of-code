# --- Day 16: Proboscidea Volcanium ---

input <- readLines("2022/data/test16.txt")

gsub('(Valve| has flow rate=|; tunne[ls]+ lea[ds]+ to valv[es]+)',
     ';',
     input) |>
  strsplit(';') |>
  lapply(\(x) {
    c(x[2:3], strsplit(x[4], ','))
  })
