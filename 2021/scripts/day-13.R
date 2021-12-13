# --- Day 13: Transparent Origami ---

input <- readLines('2021/input/input-13.txt')

# Part 1 and 2
fold <- read.table(text = gsub('fold along ', '', input[grepl('^f', input)]), sep = '=')
dots <- read.table(text = input[grepl('^[^f]', input)], sep = ',', col.names = c('x', 'y'))

for(i in seq(nrow(fold))) {
  dots[dots[fold[i,1]] > fold[i,2], fold[i,1]] <- 2*fold[i,2] - dots[dots[fold[i,1]] > fold[i,2], fold[i,1]]
  if(i == 1) print(nrow(unique(dots)))
}

plot(dots, ylim = rev(range(dots$x)))
