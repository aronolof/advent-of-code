input <- read.csv('2024/data/input01.txt', sep = ' ', header = F)

# Part 1
sum(abs(sort(input$V1) - sort(input$V4)))

# Part 2
table(input$V4)[as.character(input$V1)] |>
  (\(x) {sum(as.integer(names(x)) * x, na.rm = TRUE)})()
