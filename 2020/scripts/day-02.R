# --- Day 2: Password Philosophy ---

input <- read.table(textConnection(gsub("(-|:? )", ",", readLines("2020/input/input-02.txt"))),
                    sep = ",")


# Part 1
sum(apply(input, 1, function(x) {
  `&`(sum(x[3] == unlist(strsplit(x[4], ""))) >= as.numeric(x[1]),
      sum(x[3] == unlist(strsplit(x[4], ""))) <= as.numeric(x[2]))
}))


# Part 2
sum(apply(input, 1, function(x) {
  sum(xor(x[3] == unlist(strsplit(x[4], ""))[as.numeric(x[1])],
          x[3] == unlist(strsplit(x[4], ""))[as.numeric(x[2])]))
}))
