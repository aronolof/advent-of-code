# --- Day 11: Corporate Policy ---

find_next_valid_password <- function(current_password) {
  
  # Helper function
  dec_to_base26 <- function(x) {
    sapply(7:0, \(e) {
      (x %% 26^(e + 1)) %/% (26^e)
    })
  }
  
  pw_to_base26 <- function(x) {
    match(strsplit(x, '')[[1]], letters) - 1
  }
  
  base26_to_dec <- function(x) {
    sum(x * (26^(7:0)))
  }
  
  base26_to_pw <- function(x) {
    paste0(letters[x + 1], collapse = '')
  }
  
  meets_password_conditions <- function(x) {
    condition_1 <- !any(c(8, 11, 14) %in% x)
    if (!condition_1) return(FALSE)
    
    condition_2 <- (head(x, -1) == tail(x, -1)) |>
      which() |>
      (\(j) {
        if (length(j) == 0) return(FALSE)
        diff(range(j)) >= 2
      })()
    if (!condition_2) return(FALSE)
    
    condition_3 <- sapply(1:5, \(j) {
      all(diff(x[j:(j + 2)]) == c(1, 1))
    }) |>
      any()
    if (!condition_3) return(FALSE)
    
    return(TRUE)
  }
  
  # Iterate from the current password until finding the next one
  i <- current_password |>
    pw_to_base26() |>
    base26_to_dec()
  
  while (TRUE) {
    i <- i + 1
    if (meets_password_conditions(dec_to_base26(i))) {
      return(base26_to_pw(dec_to_base26(i)))
    }
  }
}

# Part 1
part_1_answer <- find_next_valid_password('hxbxwxba')
part_1_answer

# Part 2
find_next_valid_password(part_1_answer)
