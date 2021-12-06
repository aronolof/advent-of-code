# --- Day 5: Hydrothermal Venture ---

input <- read.table(text = gsub('->', ',', readLines('2021/input/input-05.txt')), sep = ',')

# Part 1
count_overlaps <- function(segment_list) {
  vents <- lapply(segment_list,
                  \(x) {
                    mat <- matrix(0, 1000, 1000)
                    line_sign <- sign(prod(x[[1]]-x[[3]], x[[2]]-x[[4]]))
                    mat[row(mat) %in% x[[1]]:x[[3]] &
                       col(mat) %in% x[[2]]:x[[4]] &
                       list(T, row(mat) - x[[1]] == line_sign * (col(mat) - x[[2]]))[[abs(line_sign)+1]]
                   ] <- 1
                   mat
                   })
  sum(Reduce('+', vents) >= 2)
  }

input[input$V1 == input$V3 | input$V2 == input$V4,] |>
  (\(y) split(y, seq(nrow(y))))() |>
  count_overlaps()

# Part 2
split(input, seq(nrow(input))) |>
  count_overlaps()
