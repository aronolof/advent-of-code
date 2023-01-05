# --- Day 2: Inventory Management System ---
input <- readLines('2018/data/input02.txt')

# Part 1
prod(sapply(2:3, \(i) sum(sapply(input, \(x) max(table(strsplit(x, '')) == i)))))

# Part 2
split_input = strsplit(input, '')

sapply(split_input, \(x) {
  sort(sapply(split_input, \(y) {
    ifelse(sum(y == x) == length(x) - 1, paste(y[y == x], collapse = ''), NA)
  }))
}) |> 
  unlist()
