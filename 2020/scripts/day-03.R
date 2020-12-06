# --- Day 3: Toboggan Trajectory ---

input <- readLines("2020/input/input-03.txt")

# Part 1
sum(sapply(seq(length(input)),
           function(x) unlist(str_split(input[x], ""))[(((x-1)*3) %% 31)+1] == "#"))


# Part 2
encounters <- function(right, down) {
  sum(sapply(seq(1, length(input), down),
             function(x) {
               unlist(str_split(input[x], ""))[(((x - 1) * right * down) %% 31) + 1] == "#"
             }))
}

prod(mapply(encounters,
            c(1, 3, 5, 7, 1),
            c(1, 1, 1, 1 , 2)))
 