# --- Day 16: Aunt Sue ---

target_aunt <- "children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1" |>
  strsplit('[^a-z0-9]+') |>
  unlist() |>
  (\(x) setNames(as.numeric(x[c(F, T)]), x[c(T, F)]))()

aunts_attributes <- readLines('2015/data/input16.txt') |>
  strsplit('[ ,:]+') |>
  lapply(\(x) {
    setNames(as.numeric(x[c(4,6,8)]), x[c(3,5,7)])
  })

# Part 1
aunts_attributes |>
  sapply(\(x) {
    all(target_aunt[names(x)] == x)
  }) |>
  which()

# Part 2
aunts_attributes |>
  sapply(\(x) {
    greater <- c('cats', 'trees')
    less <- c('pomeranians', 'goldfish')
    all(
      target_aunt[intersect(names(x), greater)] < x[intersect(names(x), greater)],
      target_aunt[intersect(names(x), less)] > x[intersect(names(x), less)],
      target_aunt[setdiff(names(x), c(greater, less))] == x[setdiff(names(x), c(greater, less))]
    )
  }) |>
  which()
