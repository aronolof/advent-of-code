input <- readLines("2022/data/input13.txt")[c(T,T,F)] |>
  lapply(\(x) {
    gsub(']', ')', gsub('\\[', 'list(', x)) |>
    parse(text = _) |>
    eval()
    })

comparisons <- function(x1, x2) {
  lengths <- c(length(x1), length(x2))
  if (sum(lengths) == 0) return('')
  for (i in seq(max(lengths))) {
    if (i > length(x1)) return('right')
    if (i > length(x2)) return('wrong')
    if (typeof(x1[[i]]) == 'double' & typeof(x2[[i]]) == 'double') {
      if (x1[[i]] < x2[[i]]) return('right')
      if (x1[[i]] > x2[[i]]) return('wrong')
      
      if (x1[[i]] == x2[[i]]) next
    }
    
    if (typeof(x1[[i]]) == 'list' & typeof(x2[[i]]) == 'list') {
      result <- comparisons(x1[[i]], x2[[i]])
      if (result == '') next
      return(result)
    }
    
    if (typeof(x1[[i]]) == 'list' & typeof(x2[[i]]) == 'double') {
      result <- comparisons(x1[[i]], list(x2[[i]]))
      if (result == '') next
      return(result)
    }
    
    if (typeof(x1[[i]]) == 'double' & typeof(x2[[i]]) == 'list') {
      result <- comparisons(list(x1[[i]]), x2[[i]])
      if (result == '') next
      return(result)
    }
  }
  return('')
}

(sapply(seq(1, length(input), 2), \(p) comparisons(input[[p]], input[[p + 1]])) == 'right') %>%
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
  sum(sapply(append(input, dividers[[2]]), \(x) comparisons(dividers[[1]], x)) == 'wrong') + 1,
  sum(sapply(append(input, dividers[[1]]), \(x) comparisons(dividers[[2]], x)) == 'wrong') + 1
)
