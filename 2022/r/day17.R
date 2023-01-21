rock_types <- list(
  c((0:3) + 0i),
  c(1 + 0i, (0:2) + 1i, 1 + 2i),
  c(2 + 2i, 2 + 1i, (0:2) + 0i),
  c((0:3) * 1i),
  c((0:1) + 0i, (0:1) + 1i)
)

rock_types

input <- strsplit(readLines("2022/data/test17.txt"), '')[[1]]
dirs <- c(">" = 1, "<" = -1)[input]

i = 0
bottom_edge <- 0i
settled_rocks <- (0:6) + bottom_edge
r <- 0
current_rock <- rock_types[[(r %% length(rock_types)) + 1]] + 2 + (bottom_edge + 4i)

height <- Im(bottom_edge)

while (TRUE) {

  # Try move to the side
  jet_stream <- dirs[(i %% length(dirs)) + 1]
  try_push <- current_rock + jet_stream
  
  stuck <- any(
    any(try_push %in% settled_rocks),
    any(Re(try_push) < 0),
    any(Re(try_push) > 6)
    )
  current_rock <- current_rock + ifelse(stuck, 0, jet_stream)
  
  # Try going downwards
  try_fall <- current_rock - 1i
  if (any(try_fall %in% settled_rocks)) {
    settled_rocks <- append(settled_rocks, current_rock)
    
    r <- r + 1
    print(r)
    
    if (r >= 2022) {
      print(max(Im(settled_rocks))) 
      break
    }
    bottom_edge <- max(Im(settled_rocks)) * 1i
    current_rock <- rock_types[[(r %% length(rock_types)) + 1]] + 2 + (bottom_edge + 4i)
    
    height <- append(height, Im(bottom_edge))
    
  } else {
    current_rock <- current_rock - 1i
  }
  
  i <- i + 1

}

# Part 2 todo
diff(height)
length(input)
length(rock_types)

