# --- Day 7: Some Assembly Required ---
input <- readLines("2015/data/input07.txt") |>
  strsplit(' -> ')

# Part 1
circuit <- setNames(sapply(input, \(x) x[1]),
                    sapply(input, \(x) x[2]))

run_circuit <- function(circuit) {
  while (sum(is.na(suppressWarnings(as.numeric(circuit)))) > 0) {
    
    for (i in seq(circuit)) {
      x <- circuit[i]
      
      if (grepl('AND', x)) {
        
        vars <- strsplit(x, ' ')[[1]][c(1, 3)]
        vals <- sapply(vars, \(y) {
          ifelse(is.na(as.numeric(y)), circuit[y], y)
        }) |>
          as.numeric() |>
          suppressWarnings()
        
        
        if (all(!is.na(vals))) {
          
          circuit[i] <- bitwAnd(vals[1], vals[2])
        }
        
      } else if (grepl('OR', x)) {
        
        vars <- strsplit(x, ' ')[[1]][c(1, 3)]
        vals <- sapply(vars, \(y) {
          ifelse(is.na(as.numeric(y)), circuit[y], y)
        }) |>
          as.numeric() |>
          suppressWarnings()
        
        if (all(!is.na(vals))) {
          
          circuit[i] <- bitwOr(vals[1], vals[2])
        }
        
      } else if (grepl('LSHIFT', x)) {
        
        vars <- strsplit(x, ' ')[[1]][c(1, 3)]
        vals <- as.numeric(circuit[vars[1]]) |>
          suppressWarnings()
        
        if (!is.na(vals)) {
          
          circuit[i] <- bitwShiftL(vals, as.numeric(vars[2]))
        }
        
      } else if (grepl('RSHIFT', x)) {
        
        vars <- strsplit(x, ' ')[[1]][c(1, 3)]
        vals <- as.numeric(circuit[vars[1]]) |>
          suppressWarnings()
        
        if (!is.na(vals)) {
          
          circuit[i] <- bitwShiftR(vals, as.numeric(vars[2]))
        }
        
      } else if (grepl('NOT', x)) {
        vars <- strsplit(x, ' ')[[1]][2]
        vals <- as.numeric(circuit[vars]) |>
          suppressWarnings()
        
        if (!is.na(vals)) {
          
          circuit[i] <- sum(2 ^ (0:15) * as.numeric((intToBits(vals)[1:16] == 0)))
        }
      } else {
        vals <- suppressWarnings(as.numeric(circuit[x]))
        
        if (!is.na(vals)) {
          
          circuit[i] <- vals
        }
      }
    }
    #print(sum(is.na(suppressWarnings(as.numeric(circuit)))))
  }
  return(as.numeric(circuit['a']))
}

part_1 <- run_circuit(circuit)
part_1

# Part 2
replace(circuit, names(circuit) == 'b', part_1) |>
  run_circuit()
