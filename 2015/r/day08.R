# --- Day 8: Matchsticks ---
input <- readLines('2015/data/input08.txt')

# Part 1
sum(
  nchar(input),
  2 - nchar(gsub('(\\\\\\\\|\\\\"|\\\\x[0-9a-f][0-9a-f])', 'X', input))
  )

# Part 2
sum(
  nchar(gsub('("|\\\\)', 'XX', input)),
  2 - nchar(input)
)
