# --- Day 8: Seven Segment Search ---

input <- read.table('2021/input/input-08.txt', sep = ' ')[,-11]

# Part 1
apply(input[11:14], 2,
      \(x) sum(nchar(x) %in% c(2, 3, 4, 7))
      ) |> sum()

# Part 2
answer_matrix <- matrix(nrow = nrow(input), ncol = ncol(input))

for(i in 1:nrow(answer_matrix)) {

  thisrow <- t(input[i,]) |> as.vector()
  answers <- rep(NA, 14)
  
  # Find the easy ones
  answers[nchar(thisrow) == 2] <- 1
  answers[nchar(thisrow) == 4] <- 4
  answers[nchar(thisrow) == 3] <- 7
  answers[nchar(thisrow) == 7] <- 8
  
  # Find 3
  n1 <- unique(unlist(strsplit(thisrow[which(answers == 1)], '')))
  answers[apply(sapply(n1, \(x) grepl(x, thisrow)), 1, sum) == 2 & nchar(thisrow) == 5] <- 3
  
  # Find 6
  answers[apply(sapply(n1, \(x) grepl(x, thisrow)), 1, sum) == 1 & nchar(thisrow) == 6] <- 6
  
  # Find 9
  n3 <- unique(unlist(strsplit(thisrow[which(answers == 3)], '')))
  answers[apply(sapply(n3, \(x) grepl(x, thisrow)), 1, sum) == 5 & nchar(thisrow) == 6] <- 9
  
  # Find 5
  n6 <- unique(unlist(strsplit(unique(thisrow[which(answers == 6)]), '')))
  answers[apply(sapply(n6, \(x) grepl(x, thisrow)), 1, sum) == 5 & nchar(thisrow) == 5] <- 5
  
  # Find 0
  n4 <- unique(unlist(strsplit(thisrow[which(answers == 4)], '')))
  answers[apply(sapply(setdiff(n4, n1), \(x) grepl(x, thisrow)), 1, sum) == 1 & nchar(thisrow) == 6] <- 0
  
  # Find 2
  n5 <- unique(unlist(strsplit(thisrow[which(answers == 5)], '')))
  answers[apply(sapply(n5, \(x) grepl(x, thisrow)), 1, sum) == 3 & nchar(thisrow) == 5] <- 2
  
  if(length(setdiff(c(2, 3, 5), answers[nchar(thisrow) == 5])) == 1){
    answers[nchar(thisrow) == 5 & is.na(answers)] <- setdiff(c(2, 3, 5), answers[nchar(thisrow) == 5])
  }
  
  if(length(setdiff(c(0, 6, 9), answers[nchar(thisrow) == 6])) == 1){
    answers[nchar(thisrow) == 6 & is.na(answers)] <- setdiff(c(0, 6, 9), answers[nchar(thisrow) == 6])
  }
  
  answer_matrix[i,] <- answers
  
}

sum(as.numeric(apply(answer_matrix[,11:14], 1, paste, collapse='')))
