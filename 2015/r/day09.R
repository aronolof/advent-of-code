# --- Day 9: All in a Single Night ---
  
input <- read.table('2015/data/input09.txt')

distances <- mapply(c, input[c(1,3,5)], input[c(3,1,5)]) |>
  as.data.frame() |>
  setNames(c('origin', 'destination', 'dist'))
distances$dist <- as.integer(distances$dist)

find_best_route <- function(distances, part = 1) {
  
  n_locations <- length(unique(distances$origin))
  
  traverse_locations <- function(current, visited, distance_traveled) {
    routes <- distances[distances$origin == current & !(distances$destination %in% visited),]
    
    if (nrow(routes) == 0) {
      return(distance_traveled)
      if (length(visited) == n_locations) {
        return(distance_traveled)
      } else {
        return(Inf)
      }
      
    } else {
      route_distances <- sapply(seq(nrow(routes)), \(i) {
        traverse_locations(routes$destination[i],
                           c(visited, routes$destination[i]),
                           distance_traveled + routes$dist[i])
      })
      
      if (part == 2) return(max(route_distances))
      return(min(route_distances))
    }
    
  }
  
  route_distances <- sapply(unique(distances$origin), \(s) traverse_locations(s, s, 0))
  ifelse(part == 2, max(route_distances), min(route_distances))
}

# Part 1
find_best_route(distances)

# Part 2
find_best_route(distances, part = 2)
