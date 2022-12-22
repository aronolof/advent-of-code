# --- Day 21: Monkey Math ---
input <- readLines('2022/data/test21.txt') 

# Part 1
monkey_math <- function(input) {
  monkeys <- input |>
    strsplit(': ') |>
    sapply(\(x) setNames(x[2], x[1]))
  
  while (TRUE) {
    subs <- suppressWarnings(monkeys[!is.na(as.numeric(monkeys))])
    for (i in seq(subs)) monkeys <- gsub(names(subs[i]), subs[i], monkeys)
    monkeys <- suppressWarnings(monkeys[is.na(as.numeric(monkeys))])
    
    monkeys <- sapply(monkeys, \(x) {
      if (grepl('[a-z]', x)) {
        x
      } else {
        eval(parse(text = x))
      }
    })
    if (length(subs) == length(monkeys)) return(monkeys['root'])
  }
}

monkey_math(input) |>
  paste()
