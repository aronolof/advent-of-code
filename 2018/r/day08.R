# --- Day 8: Memory Maneuver ---
input <- scan('2018/data/test08.txt')

# Part 1
metadata_entries <- c()

traverse_nodes <- function(p) {
  metadata_value <- 0
  
  n_children <- input[p]
  n_meta <- input[p + 1]
  
  p <- p + 2
  for (i in rep(1, n_children)) {
    child_node <- traverse_nodes(p)
    p <- child_node[1]
    metadata_value <- metadata_value + child_node[2]
  }
  
  metadata_value <- metadata_value + sum(input[p + seq(n_meta) - 1])
  
  return(c(p + n_meta, metadata_value))
}

traverse_nodes(1)[2]
