# --- Day 1: Chronal Calibration ---
input <- as.numeric(readLines('2018/input/input-01'))

# Part 1
sum(input)

# Part 2
(\(input, cumul = 0, appeared = 0) {
  cumul = cumsum(input) + tail(cumul, 1)
  matches = cumul[cumul %in% appeared]
  
  if (length(matches) > 0) return(matches[1])
  
  appeared = unique(c(appeared, cumul))
  Recall(input, cumul, appeared)
})(input)
