# --- Day 16: Proboscidea Volcanium ---
library(igraph)

input <- readLines("2022/data/input16.txt")

edges <- input |>
  strsplit('[^A-Z]+') |>
  lapply(\(x) lapply(x[-1:-2], \(y) c(x[2], y))) |>
  unlist() |>
  make_graph() |>
  distances()

valves <- input |>
  strsplit('[^A-Z0-9]+') |>
  sapply(\(x) setNames(as.numeric(x[3]), x[2]))

current_pos = 'AA'
minute = 1
remaining_valves <- valves[valves != 0]

make_decision <- function(current_pos, minute, remaining_valves) {
  if (minute > 30) {
    return(0)
  } else {
    max_release <- 0
    
    travel_times <- setNames(edges[rownames(edges) == current_pos], colnames(edges))
    travel_times <- travel_times[names(travel_times) %in% names(remaining_valves)]

    reachable_valve_names <- names(travel_times)[travel_times < (30 - minute )]
    reachable_valves <- remaining_valves[names(remaining_valves) %in% reachable_valve_names]
    
    for (next_valve in names(reachable_valves)) {
      
      next_valve_release <- max(30 - minute - travel_times[next_valve], 0) * reachable_valves[next_valve]
      
      next_step <- make_decision(next_valve,
                                 minute + travel_times[next_valve] + 1,
                                 remaining_valves[names(remaining_valves) != next_valve])
      
      max_release <- max(max_release, next_valve_release + next_step)
    }

    return(max_release)
  }
}

# Part 1
make_decision('AA', 1, valves[valves != 0])
