# --- Day 11: Monkey in the Middle ---
input <- readLines("2022/data/input11.txt") 

initial_state <- function(input) {
  split(input, cumsum(grepl('Monkey', input))) |>
    lapply(\(x) {
      list(n_inspections = 0,
           items = as.numeric(strsplit(gsub('  Starting items: ', '', x[2]), ',')[[1]]),
           operation = gsub('  Operation: new = ', '', x[3]),
           test = as.integer(gsub('  Test: divisible by ', '', x[4])),
           true = as.integer(gsub('    If true: throw to monkey ', '', x[5])),
           false = as.integer(gsub('    If false: throw to monkey ', '', x[6])))
    })
}
# Part 1
state <- initial_state(input)

for (round in 1:20) {
  if (round %% 50 == 0) print(round)
  for (i in seq(state)) {
    
    if (length(state[[i]]$items) > 0) {
      for (j in seq(length(state[[i]]$items))) {
        
        item = state[[i]]$items[j]
        worry_level <- gsub('old', as.character(item), state[[i]]$operation) |>
          parse(text = _) |>
          eval()
        
        worry_level <- worry_level %/% 3
        is_divisible <- (worry_level %% state[[i]]$test) == 0
        
        if (is_divisible) {
          throw_to <- state[[i]]$true + 1
        } else {
          throw_to <- state[[i]]$false + 1
        }
        state[[throw_to]]$items <- append(state[[throw_to]]$items, worry_level)
        state[[i]]$n_inspections <- state[[i]]$n_inspections + 1
      }
      state[[i]]$items <- NULL
    }
  }
}
sapply(state, \(x) x$n_inspections) |>
  sort() |>
  tail(2) |>
  prod()

# Part 2
state <- initial_state(input)
lcd <- prod(sapply(state, \(x) x$test))

for (round in seq(10000)) {
  if (round %% 100 == 0) print(round)
  for (i in seq(state)) {
    
    if (length(state[[i]]$items) > 0) {
      for (j in seq(length(state[[i]]$items))) {
        
        item = state[[i]]$items[j]
        worry_level <- gsub('old', as.character(item), state[[i]]$operation) |>
          parse(text = _) |>
          eval()
        
        worry_level <- worry_level %% lcd
        is_divisible <- (worry_level %% state[[i]]$test) == 0
        
        if (is_divisible) {
          throw_to <- state[[i]]$true + 1
        } else {
          throw_to <- state[[i]]$false + 1
        }
        state[[throw_to]]$items <- append(state[[throw_to]]$items, worry_level)
        state[[i]]$n_inspections <- state[[i]]$n_inspections + 1
      }
      state[[i]]$items <- NULL
    }
  }
}
sapply(state, \(x) x$n_inspections) |>
  sort() |>
  tail(2) |>
  prod()
