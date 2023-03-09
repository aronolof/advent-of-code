# --- Day 18: Like a GIF For Your Yard ---

input <- scan('2015/data/input18.txt', what = '') |>
  sapply(\(x) strsplit(x, '')) |>
  sapply(\(x) x == '#') |>
  unname()

update_lights <- function(m, part2 = FALSE) {
  m_nrow <- nrow(m)
  m_ncol <- ncol(m)
  
  corners <- \(m) {
    col(m) %in% c(1, 100) & row(m) %in% c(1, 100)
  }
  
  m <- m | (part2 & corners(m))
  
  m_new <- cbind(m[,-1], 0) +
    cbind(0, m[,-m_ncol]) +
    rbind(m[-1,], 0) +
    rbind(0, m[-m_nrow,]) +
    cbind(rbind(m[-1,], 0)[,-1], 0) +
    cbind(rbind(0, m[-m_nrow,])[,-1], 0) +
    cbind(0, rbind(m[-1,], 0)[,-m_ncol]) +
    cbind(0, rbind(0, m[-m_nrow,])[,-m_ncol])
  
  (m == 1 & m_new %in% c(2,3)) |
    (m == 0 & m_new == 3) |
    (part2 & corners(m_new))
}

# Part 1
(\(m) {
  for (i in 1:100) m <- update_lights(m)
  sum(m)
})(input)

# Part 2
(\(m) {
  for (i in 1:100) m <- update_lights(m, part2 = TRUE)
  sum(m)
})(input)
