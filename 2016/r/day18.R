# --- Day 18: Like a Rogue ---
input <- strsplit(readLines('2016/data/input18.txt'), '')[[1]]

# Part 1
count_safe_tiles <- function(input, n_rows = 40) {
  this_row <- input == '^'
  safe_count <- sum(!this_row)
  
  for (j in seq(n_rows - 1) + 1) {
    
    this_row <- c(FALSE, this_row, FALSE)
    new_row <- sapply(seq(length(this_row) - 2) + 1,
                      \(i) {
                        any(
                          this_row[i] & this_row[i - 1] & !this_row[i + 1],
                          this_row[i] & this_row[i + 1] & !this_row[i - 1],
                          !(this_row[i] | this_row[i - 1]) & this_row[i + 1],
                          !(this_row[i] | this_row[i + 1]) & this_row[i - 1]
                        )
                      })
    
    safe_count <- safe_count + sum(!new_row)
    this_row <- new_row
    if ((j %% 1000) == 0) print(paste('row:', j))
  }
  print(paste('row:', j))
  return(safe_count)
}

count_safe_tiles(input, 40)

# Part 2, brute forced;
count_safe_tiles(input, 400000)

