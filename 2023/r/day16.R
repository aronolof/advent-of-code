# --- Day 16: The Floor Will Be Lava ---

input <- readLines('2023/data/input16.txt') |>
  strsplit('') |>
  do.call(rbind, args = _)

sum_energized <- function(start_pos, direction) {
  m <- input == ''
  travel <- matrix(c(start_pos, direction), ncol = 2)
  i <- 1
  
  while(i <= nrow(travel)) {
    #print(i)
    travel_row <- travel[i,]
    current_pos <- travel_row[1] + travel_row[2]
    
    repeat {
      if(Re(current_pos) <= 0 || Im(current_pos) <= 0) break
      if(Re(current_pos) > nrow(input) || Im(current_pos) > ncol(input)) break
      
      m[Re(current_pos), Im(current_pos)] <- TRUE
      current_symbol <- input[Re(current_pos), Im(current_pos)]
      if (current_symbol != '.') {
  
        if (current_symbol == '|' && Im(travel_row[2]) != 0) {
          
          if(!all(travel[,1] == current_pos & travel[,2] == 1+0i)) {
            travel <- rbind(travel, c(current_pos, 1+0i))
          }
          if(!all(travel[,1] == current_pos & travel[,2] == -1+0i)) {
            travel <- rbind(travel, c(current_pos, -1+0i))
          }
          
          break
        } else if (current_symbol == '-' && Re(travel_row[2]) != 0) {
          
          if(!all(travel[,1] == current_pos & travel[,2] == 0+1i)) {
            travel <- rbind(travel, c(current_pos, 0+1i))
          }
          if(!all(travel[,1] == current_pos & travel[,2] == 0-1i)) {
            travel <- rbind(travel, c(current_pos, 0-1i))
          }
          break
          
        } else if (current_symbol == '-' && Im(travel_row[2]) != 0) {
          current_pos <- current_pos + travel_row[2]
        } else if (current_symbol == '|' && Re(travel_row[2]) != 0) {
          current_pos <- current_pos + travel_row[2]
        } else if (current_symbol == '/') {
          if (Im(travel_row[2]) == 1 || Re(travel_row[2]) == 1) {
            
            if (!any(current_pos == travel[,1] & (travel_row[2] - 1-1i) == travel[,2])) {
              travel <- rbind(travel, c(current_pos, (travel_row[2] - 1-1i)))
            }
          } else {
            if (!any(current_pos == travel[,1] & (travel_row[2] + 1+1i) == travel[,2])) {
              travel <- rbind(travel, c(current_pos, (travel_row[2] + 1+1i)))
            }
          }
          break
        } else if (current_symbol == '\\') {
          if (Im(travel_row[2]) == 1 || Re(travel_row[2]) == -1) {
            
            if (any(current_pos == travel[,1] & (travel_row[2] + 1-1i) == travel[,2])) {
              #print('current pos already exists')
              break
            }
            travel <- rbind(travel, c(current_pos, (travel_row[2] + 1-1i)))
            
          } else {
            if (any(current_pos == travel[,1] & (travel_row[2] - 1+1i) == travel[,2])) {
              #print('current pos already exists')
              break
            }
            travel <- rbind(travel, c(current_pos, (travel_row[2] - 1+1i)))
          }
          break
        } else {
          #print('unknown event')
          #print(current_symbol)
          break
        }
        
      } else {
        current_pos <- current_pos + travel_row[2]
      }
    }
    
    i <- i + 1
  }
  sum(m)
}
sum_energized(1+0i, 0+1i)

c(
  mapply(sum_energized, start_pos = seq(nrow(input)) + 0i, direction = 0+1i) |> max(),
  mapply(sum_energized, start_pos = seq(nrow(input)) + complex(imaginary = ncol(input) + 1), direction = 0-1i) |> max(),
  mapply(sum_energized, start_pos = 0 + complex(imaginary = seq(ncol(input))), direction = 1+0i) |> max(),
  mapply(sum_energized, start_pos = nrow(input) + 1 + complex(imaginary = seq(ncol(input))), direction = -1+0i) |> max()
) |>
  max()