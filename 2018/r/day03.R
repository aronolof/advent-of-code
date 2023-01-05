# --- Day 3: No Matter How You Slice It ---

input <- readLines('2018/data/input03.txt')

# Part 1
df <- t(sapply(strsplit(gsub('#|x|: | @ ', ',', input), ','), as.numeric))
fabric <- matrix(0, max(df[,4] + df[,6]), max(df[,3] + df[,5]))

for (i in seq(nrow(df))) {
  claim <- df[i,]
  x <- (claim[3] + 1):(claim[3] + claim[5])
  y <- (claim[4] + 1):(claim[4] + claim[6])
  fabric[x, y] <- fabric[x, y] + 1
}
sum(fabric >= 2)

# Part 2
for(i in seq(nrow(df))) {
  claim <- df[i,]
  x <- (claim[3]+1):(claim[3]+claim[5])
  y <- (claim[4]+1):(claim[4]+claim[6])
  if(all(fabric[x, y] == 1)) {
    print(claim[2])
    break
  }
}
