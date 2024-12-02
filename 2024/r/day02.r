input <- readLines('2024/data/input02.txt') |>
  strsplit(' ') |>
  sapply(as.integer)

# Part 1
is_safe <- function(x) {
  a <- range(diff(as.integer(x)))
  return((a[1] >= -3 & a[2] <= -1) || (a[1] >= 1 & a[2] <= 3))
}

input |>
  sapply(is_safe) |>
  sum()

# Part 2
sapply(input, 
       \(x) {
         if (is_safe(x)) return(TRUE)

         for(i in seq_along(x)) {
           if (is_safe(x[-i])) return(TRUE)
         }
         
         return(FALSE)
       }
) |>
  sum()
