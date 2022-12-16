# --- Day 13: Distress Signal ---
input <- readLines("2022/data/input13.txt")[c(T,T,F)] |>
  lapply(\(x) {
    gsub('\\[', 'list(', x) |>
    gsub(']', ')', x = _) |>
    parse(text = _) |>
    eval()
    })

packet_comparison <- function(x, y) {
  if (length(x) + length(y) == 0) return(NULL)
  
  for (i in seq(max(c(length(x), length(y))))) {
    
    if (i > length(x)) return(TRUE)
    if (i > length(y)) return(FALSE)
    
    if (typeof(x[[i]]) == 'double' & typeof(y[[i]]) == 'double') {
      if (x[[i]] < y[[i]]) return(TRUE)
      if (x[[i]] > y[[i]]) return(FALSE)
      if (x[[i]] == y[[i]]) next
      
    } else if (typeof(x[[i]]) == 'list' & typeof(y[[i]]) == 'list') {
      result <- packet_comparison(x[[i]], y[[i]])
      
    } else if (typeof(x[[i]]) == 'list' & typeof(y[[i]]) == 'double') {
      result <- packet_comparison(x[[i]], list(y[[i]]))
      
    } else if (typeof(x[[i]]) == 'double' & typeof(y[[i]]) == 'list') {
      result <- packet_comparison(list(x[[i]]), y[[i]])
    }
    
    if (is.null(result)) {
      next
    } else {
      return(result)
    }
  }
  return(NULL)
}

(sapply(seq(1, length(input), 2), \(p) packet_comparison(input[[p]], input[[p + 1]]))) %>%
  which() %>%
  sum()

# Part 2
dividers <- c('[[2]]', '[[6]]') |>
  lapply(\(x) {
    gsub(']', ')', gsub('\\[', 'list(', x)) |>
      parse(text = _) |>
      eval()
  })

prod(
  sum(sapply(append(input, dividers[[2]]), \(x) packet_comparison(x, dividers[[1]]))) + 1,
  sum(sapply(append(input, dividers[[1]]), \(x) packet_comparison(x, dividers[[2]]))) + 1
)
