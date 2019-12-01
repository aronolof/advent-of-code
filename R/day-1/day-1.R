# --- Day 1: The Tyranny of the Rocket Equation ---

# Problem: https://adventofcode.com/2019/day/1

# Part 1 solution

sum(floor(scan("input.txt")/3-2))

# Part 2 solution

sum(sapply(scan("input.txt"),
           function(x) {
             fuel <- floor(x / 3) - 2
             if (fuel <= 0) {
               return(0)
             } else {
               return(fuel + Recall(fuel))
             }
           }))




