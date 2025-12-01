input <- readLines("2025/data/input01.txt")

# Part 1
x <- gsub("L", "-", input) |>
    gsub("R", "", x = _) |>
    as.integer() |>
    cumsum()

sum(((x + 50) %% 100) == 0)
