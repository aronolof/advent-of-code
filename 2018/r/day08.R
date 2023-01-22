# --- Day 8: Memory Maneuver ---
input <- scan('2018/data/input08.txt')

# Part 1
traverse_nodes_pt1 <- function(input, p) {
  value <- 0
  
  n_children <- input[p]
  n_meta <- input[p + 1]
  
  p <- p + 2
  for (i in rep(1, n_children)) {
    child_node <- traverse_nodes_pt1(input, p)
    p <- child_node[1]
    value <- value + child_node[2]
  }
  value <- value + sum(input[p + seq(n_meta) - 1])
  
  return(c(p + n_meta, value))
}

traverse_nodes_pt1(input, 1)[2]

# Part 2
traverse_nodes_pt2 <- function(input, p) {
  metadata_value <- 0
  
  n_children <- input[p]
  n_meta <- input[p + 1]
  
  p <- p + 2
  
  if (n_children == 0) {
    metadata_value <- sum(input[p + seq(n_meta) - 1])
  } else {
    
    child_values <- c()
    
    for (i in rep(1, n_children)) {
      child_node <- traverse_nodes_pt2(input, p)
      p <- child_node[1]
      child_values <- append(child_values, child_node[2])
    }
    
    metadata_value <- sum(child_values[input[p + seq(n_meta) - 1]], na.rm = TRUE)
  }
  
  return(c(p + n_meta, metadata_value))
}

traverse_nodes_pt2(input, 1)[2]
