# --- Day 23: Unstable Diffusion ---

input <- readLines("2022/data/input23.txt") |>
  strsplit('')

unstable_diffusion <- function(part = 1) {
    
  all_adjacent <- complex(imaginary = rep(-1:1, each = 3), real = rep(-1:1, 3))[-5]
  
  adjacent <- list(
    'N' = all_adjacent[c(1, 2, 3)],
    'S' = all_adjacent[c(6, 7, 8)],
    'W' = all_adjacent[c(1, 4, 6)],
    'E' = all_adjacent[c(3, 5, 8)]
  )
  
  elf_positions <- lapply(seq(input), \(i) {
    if (sum(input[[i]] == '#') == 0) return(NULL)
    complex(real = which(input[[i]] == '#'), imaginary = i)
    }) |>
    unlist()
  
  j <- 0
  while (TRUE) {
    j <- j + 1
    proposed_moves <- sapply(elf_positions, \(pos) {
      
      adjacent_positions <- elf_positions[
        abs(Re(elf_positions) - Re(pos)) <= 1 &
          abs(Im(elf_positions) - Im(pos)) <= 1
          #elf_positions != pos
      ]
      
      #if (length(adjacent_positions) == 0) return(pos)
      
      # If none surrounding, stay at current position
      if (!any((pos + all_adjacent) %in% adjacent_positions)) return(pos)
      
      # 
      for (adj in adjacent) {
        if (sum((pos + adj) %in% adjacent_positions) == 0) return(pos + adj[2])
      }
      
      # Stay at current positions if none other available
      return(pos)
    })
    
    # Check part 2
    if (part != 1) {
      if ((j %% 5) == 0) {
        print(paste0('Round: ', j, ' | Not moving: ', round(mean(proposed_moves == elf_positions) * 100, 2), '%'))
      }
      
      if (all(proposed_moves == elf_positions)) {
        return(paste0('Round: ', j, ' | Not moving: ', round(mean(proposed_moves == elf_positions) * 100, 2), '%'))
      }
    }
    
    # Update positions
    collisions <- duplicated(proposed_moves) | duplicated(proposed_moves, fromLast = T)
    elf_positions[!collisions] <- proposed_moves[!collisions]
    
    # Rotate the direction preference
    adjacent <- c(adjacent[-1], adjacent[1])
    
    # Check part 1
    if (part == 1 & j == 10) {
      width <- diff(range(Re(elf_positions))) + 1
      height <- diff(range(Im(elf_positions))) + 1
      empty_tiles <- (width * height) - length(elf_positions)
      return(paste0('Round: ', j, ' | Empty tiles: ', empty_tiles))
    }
  }
}

# Part 1
unstable_diffusion()

# Part 2
unstable_diffusion(part = 2)
