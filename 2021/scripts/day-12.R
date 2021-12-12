# --- Day 12: Passage Pathing ---

input <- read.table('2021/input/input-12.txt', sep = '-')

# Part 1
alternatives <- rbind(input, setNames(input, names(input)[2:1])) |>
  (\(x) x[!(x$V1 %in% c('end', 'start')) & x$V2 != 'start',])()

small_caves <- (\(x) x[nchar(x) < 3 & tolower(x) == x])(unique(alternatives$V1))

paths <- rbind(setNames(input[input$V1 == 'start',], 1:2),
               setNames(input[input$V2 == 'start',], 2:1))

repeat{
  
  next_paths <- setNames(alternatives, ncol(paths):(ncol(paths)+1))
 
  paths <- merge(x = paths, y = next_paths, all.x = TRUE)
  paths <- paths[,paste(seq(ncol(paths)))]
  paths <- paths[apply(paths, 1, \(x) sum(duplicated(x) & x %in% small_caves) == 0),]
  
  if(all(is.na(paths[[ncol(paths)]]))) break
}

nrow(paths)

# Part 2
paths <- rbind(setNames(input[input$V1 == 'start',], 1:2),
               setNames(input[input$V2 == 'start',], 2:1))

repeat{
  
  next_paths <- setNames(alternatives, ncol(paths):(ncol(paths)+1))
  
  paths <- merge(x = paths, y = next_paths, all.x = TRUE)
  paths <- paths[,paste(seq(ncol(paths)))]
  paths <- paths[apply(paths, 1, \(x) sum(duplicated(x) & x %in% small_caves) <= 1),]
  
  if(all(is.na(paths[[ncol(paths)]]))) break
}

nrow(paths)
