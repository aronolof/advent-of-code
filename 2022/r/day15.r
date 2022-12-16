#input <- readLines("2022/data/test15.txt")
input <- readLines("2022/data/input15.txt")

df <- gsub(':', ',', gsub('[^-0-9,:]', '', input)) |>
  strsplit(',') |>
  sapply(as.numeric) |>
  t() |>
  as.data.frame()

# Manhattan distance to beacon
df[,5] <- abs(df[,1] - df[,3]) + abs(df[,2] - df[,4])

# Part 1
y = 2000000
apply(df, 1, \(x) {
  # distance from middle
  y_dist <- abs(y - x[2])
  x_zone <- x[5] - y_dist
  if (x_zone < 0) return(NULL)
  return((x[1] - x_zone):(x[1] + x_zone))
}) |>
  unlist() |>
  setdiff(df[df$V4 == y, 'V3']) |>
  unique() |>
  length()