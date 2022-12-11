# --- Day 11: Monkey in the Middle ---

input <- readLines("2022/data/input11.txt") 
input_list <- split(input, cumsum(grepl('Monkey', input)))

state <- lapply(input_list, \(x) {
  list(n_inspections = 0,
       items = as.numeric(strsplit(gsub('  Starting items: ', '', x[2]), ',')[[1]]),
       operation = gsub('  Operation: new = ', '', x[3]),
       test = as.integer(gsub('  Test: divisible by ', '', x[4])),
       true = as.integer(gsub('    If true: throw to monkey ', '', x[5])),
       false = as.integer(gsub('    If false: throw to monkey ', '', x[6])))
})

for (round in 1:20) {
  for (i in seq(state)) {
    print(paste(round, i))
    if (length(state[[i]]$items) > 0) {
      for (j in seq(length(state[[i]]$items))) {
        
        item = state[[i]]$items[j]
        new_worry <- gsub('old', as.character(item), state[[i]]$operation) |>
          parse(text = _) |>
          eval()
        
        new_worry <- new_worry %/% 3
        is_divisible <- (new_worry %% state[[i]]$test) == 0
        
        if (is_divisible) {
          throw_to <- state[[i]]$true + 1
        } else {
          throw_to <- state[[i]]$false + 1
        }
        state[[throw_to]]$items <- append(state[[throw_to]]$items, new_worry)
        state[[i]]$n_inspections <- state[[i]]$n_inspections + 1
      }
      state[[i]]$items <- NULL
    }
  }
}
# Part 1
sapply(seq(state), \(i) state[[i]][[1]]) |> sort(decreasing = T) |> head(2) |> prod()

