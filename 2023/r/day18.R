# 

input <- read.delim('2023/data/test18.txt', sep = ' ', header = FALSE)
input$V3 <- substr(input$V3, 2, 8)

max_dim <- sapply(c('U', 'D', 'L', 'R'), \(x) {
  input$V2[input$V1 == x] |>
    sum()
})

m <- matrix(NA, nrow = max_dim['U'] * 2, ncol = max_dim['L'] * 2)

pos <- unname(max_dim[2:3])
head(input)
for (i in seq(nrow(input))) {
  print(input[i,])
  if (input$V1[i] == 'R') {
    m[pos[1], seq(pos[2], pos[2] + input$V2[i])] <- 'R' #input$V3[i]
    pos <- pos + c(0, input$V2[i])
  }
  if (input$V1[i] == 'L') {
    m[pos[1], seq(pos[2], pos[2] - input$V2[i])] <- 'L' #input$V3[i]
    pos <- pos - c(0, input$V2[i])
  }
  if (input$V1[i] == 'D') {
    m[seq(pos[1], pos[1] + input$V2[i]), pos[2]] <- 'D' #input$V3[i]
    pos <- pos + c(input$V2[i], 0)
  }
  if (input$V1[i] == 'U') {
    m[seq(pos[1], pos[1] - input$V2[i]), pos[2]] <- 'U' #input$V3[i]
    pos <- pos - c(input$V2[i], 0)
  }
}

closed_side <- paste0(input$V1, c(input$V1[-1], input$V1[1])) |>
  table() |>
  (\(x) sum(x[c('RD', 'DL', 'LU', 'UR')]) - sum(x[c('RU', 'UL', 'LD', 'DR')]))()


closed_side

m1 <- apply(m, 2, \(x)
      split(x, cumsum(!is.na(x))) |>
        lapply(\(y) {
          if(!is.na(y[1]) && y[1] == 'R') return(rep(TRUE, length(y)))
          return(!is.na(y))
        }) |>
        unlist()
      )

m2 <- apply(m, 2, \(x)
            split(x, rev(cumsum(!is.na(rev(x))))) |> rev() |>
              lapply(\(y) {
                if(!is.na(tail(y, 1)) && tail(y, 1) == 'L') return(rep(TRUE, length(y)))
                return(!is.na(y))
              }) |>
              unlist()
)

m2 <- apply(m, 1, \(x)
            split(x, cumsum(!is.na(x))) |>
              lapply(\(y) {
                if(!is.na(y[1]) && y[1] == 'U') return(rep(TRUE, length(y)))
                return(!is.na(y))
              }) |>
              unlist()
) |>
  t()

m4 <- apply(m, 1, \(x)
            split(x, rev(cumsum(!is.na(rev(x))))) |> rev() |>
              lapply(\(y) {
                if(!is.na(tail(y, 1)) && tail(y, 1) == 'D') return(rep(TRUE, length(y)))
                return(!is.na(y))
              }) |>
              unlist()
) |>
  t()

m1 | m2
