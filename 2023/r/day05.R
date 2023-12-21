# --- Day 5: If You Give A Seed A Fertilizer ---

input <- readLines("2023/data/input05.txt")

# Part 1
seeds <- strsplit(input[1], ' ')[[1]][-1] |>
  as.numeric()

m <- strsplit(input, ' ')[grepl('[0-9]', input)][-1] |>
  sapply(as.numeric) |>
  t()
m2 <- cumsum(input == "")[grepl('[0-9]', input)][-1] |>
  cbind(m)

sapply(seeds, \(x) {
  new <- x
  for (i in unique(m2[,1])) {
    matches <- (m2[,1] == i) & (new >= m2[,3]) & (new <= m2[,3] + m2[,4] - 1)
    w <- which(matches)
    if (length(w) > 0) new <- new + diff(m2[w,3:2])
  }
  new
}) |>
  min()

