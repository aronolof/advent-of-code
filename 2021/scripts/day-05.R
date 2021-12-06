# --- Day 5: Hydrothermal Venture ---

input <- read.table(text = gsub('->', ',', readLines('2021/input/input-05.txt')), sep = ',')

# Part 1
only_hv <- input[input$V1 == input$V3 | input$V2 == input$V4,]
only_hv_list <- split(only_hv, seq(nrow(only_hv)))

matrices <- lapply(only_hv_list,
       \(x) {
         mat <- matrix(0, 1000, 1000)
         mat[x[[2]]:x[[4]], x[[1]]:x[[3]]] <- 1
         mat
       }
       )

sum(Reduce('+', matrices) >= 2)
#4873

# Part 2
input_list <- split(input, seq(nrow(input)))

matrices <- lapply(input_list,
                   \(x) {
                     mat <- matrix(0, 1000, 1000)
                     line_sign <- sign(prod(x[[1]]-x[[3]], x[[2]]-x[[4]]))
                     mat[row(mat) %in% x[[1]]:x[[3]] &
                         col(mat) %in% x[[2]]:x[[4]] &
                         list(T, row(mat) - x[[1]] == line_sign * (col(mat) - x[[2]]))[[abs(line_sign)+1]]
                     ] <- 1
                     mat
                   }
)
sum(Reduce('+', matrices) >= 2)
#19472
