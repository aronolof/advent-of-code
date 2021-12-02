# --- Day 2: Dive! ---

input <- read.table("2021/input/input-02.txt")

# Part 1
prod(aggregate(input$V2 * ifelse(input$V1 == 'up', -1, 1), by=list(input$V1 == 'forward'), FUN=sum)$x)

# Part 2
((input$V1 != 'forward') * ifelse(input$V1 == 'up', -1, 1) * input$V2) |>
  (\(x) tail(cumsum((!x) * input$V2), 1) * tail(cumsum((!x) * input$V2 * cumsum(x)), 1))()
