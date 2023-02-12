# --- Day 7: Recursive Circus ---
input <- readLines('2017/data/input07.txt') |>
  strsplit('[^a-z0-9]+')

# Part 1
x <- unlist(sapply(input, \(x) x[1]))
y <- unlist(sapply(input, \(x) x[-1:-2]))
root <- x[!(x %in% y)]
root

# Part 2
find_balance <- function(input, root) {
  tower <- setNames(input, sapply(input, \(x) x[1]))
  
  check_weights <- function(program) {
    subtowers <- tower[[program]][-1:-2]
    
    if (length(subtowers) == 0) {
      return(0)
    }
    sapply(
      subtowers,
      \(x) {
        as.integer(tower[[x]][2]) + sum(check_weights(x))
      }
    )
  }
  
  subtower_sums <- check_weights(root)
  odd_disc <- subtower_sums[subtower_sums != median(subtower_sums)]
  rebalance <- median(subtower_sums) - odd_disc
  
  while (TRUE) {
    subtower_sums <- check_weights(names(odd_disc))
    if ((length(table(subtower_sums))) == 1) {
      return(unname(as.integer(tower[[names(odd_disc)]][2]) + rebalance))
      
    } else {
      odd_disc <- subtower_sums[subtower_sums != median(subtower_sums)]
      
    }
  }
}

find_balance(input, root)
