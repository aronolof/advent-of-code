input <- scan("2023/data/input15.txt", what = character(), sep = ',') 

# Part 1
strsplit(input, '') |>
  sapply(\(x) {
    val <- 0
    for(y in x) {
      val <- ((val + utf8ToInt(y)) * 17) %% 256
    }
    val
  }) |>
  sum()

# Part 2
hash <- Vectorize(\(input) {
  strsplit(input, '') |>
    sapply(\(x) {
      val <- 0
      for(y in x) {
        val <- ((val + utf8ToInt(y)) * 17) %% 256
      }
      val
    }) |>
    sum()
})

df <- data.frame(h = hash(gsub('[^a-z]', '', input)),
                 a = gsub('[^a-z]', '', input),
                 b = gsub('[^0-9]', '', input) |> as.numeric())

split(df, df$h) |>
  sapply(\(df) {
    res <- c()
    for (i in seq(nrow(df))) {
      if(is.na(df$b[i])) {
        res <- res[names(res) != df$a[i]]
      } else if(df$a[i] %in% names(res)) {
        res[df$a[i]] <- df$b[i]
      } else {
        res <- append(res, setNames(df$b[i], df$a[i]))
      }
    }
    sum((df$h[1] + 1) * seq_along(res) * res)
  }) |>
  sum()
