# --- Day 1: The Tyranny of the Rocket Equation ---

input <- scan("2019/input/input-01.txt")

# Part 1

sum(input %/% 3 - 2)

# Part 2

sum(sapply(input,
           function(x) {
             fuel <- x %/% 3 - 2
             if (fuel <= 0) {
               return(0)
             } else {
               return(fuel + Recall(fuel))
             }
           }))
