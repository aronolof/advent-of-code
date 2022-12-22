# --- Day 21: Monkey Math ---
input <- readLines('2022/data/input21.txt') 

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
    if (length(subs) == length(monkeys)) return(paste(monkeys['root']))
  }
}

monkey_math(input)

# Part 2
monkey_math_pt2 <- function(input) {
  monkeys <- input  |>
    strsplit(': ') |>
    sapply(\(x) setNames(x[2], x[1]))
  
  monkeys['humn'] <- 'humn'
  
  while (TRUE) {
    previous_monkeys_length <- length(monkeys)
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
    
    if (length(monkeys) == previous_monkeys_length) break
  }
  
  inverse <- c('/' = '*', '*' = '/','+' = '-', '-' = '+')
  upward <- setNames(gsub('[^0-9]+', '', monkeys['root']),
                     gsub('[^a-z]+', '', monkeys['root']))
  
  while (TRUE) {
    p <- monkeys[names(upward)]
    op <- gsub('[^*/+-]', '', p)
    val <- gsub('[^0-9]', '', p)
    var <- gsub('[^a-z]', '', p)
    
    formula <- paste(upward, inverse[op], val)
    if (op == '-' & regexec('[a-z]', p)[[1]][1] > 1) formula <- paste(val, '-', upward)
    
    upward <- setNames(eval(parse(text = formula)), var)
    if (names(upward) == 'humn') return(paste(upward))
  }
}

monkey_math_pt2(input)
