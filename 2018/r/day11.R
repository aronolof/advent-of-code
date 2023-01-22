# --- Day 11: Chronal Charge ---
chronal_charge <- function(input, square_sizes = 3) {
  m <- matrix(1, nrow = 300, ncol = 300)
  
  rack_id <- col(m) + 10
  power <- rack_id * row(m)
  power <- power + input
  power <- power * rack_id
  power <- (power %/% 100) %% 10
  power <- power - 5
  
  m <- power
  
  max_power <- 0
  max_power_coords <- c()
  for (i in 1:298) {
    for (j in 2:298) {
      for (k in square_sizes) {
        if ( (k > (301 - i)) | (k > (301 - j))) break
        
        this_power <- sum(m[j + seq(k) - 1, i + seq(k) - 1])
        if (this_power > max_power) {
          max_power <- this_power
          max_power_coords <- c(i, j, k)
        }
      }
    }
  }
  max_power_coords
}

# Part 1
chronal_charge(5719)[1:2] |>
  paste(collapse = ',')

# TODO Part 2. Brute-forced way is too inefficient:
chronal_charge(5719, 1:300) |>
  paste(collapse = ',')

