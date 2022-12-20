# --- Day 4: Security Through Obscurity ---
input <- readLines("2016/data/input04.txt")

# Part 1
real_room_ids <- function(input) {
  input |>
    gsub('-', '', x = _) |>
    strsplit('') |>
    sapply(\(x) {
      
      top_letters <- x[!cumsum(x %in% 0:9)] |>
        table() |>
        sort(decreasing = TRUE) |>
        head(5)
      
      checksum <- head(tail(x, 6), 5)
      
      if (all(names(top_letters) == checksum)) {
        x[x %in% 0:9] |>
          paste(collapse = '') |>
          as.numeric()
      } else {
        NA
      }
    }) 
}

real_room_ids(input) |>
  sum(na.rm = TRUE)

# Part 2
decrypt_rooms <- function(input) {
  input |>
    strsplit('') |>
    sapply(\(x) {
      id <- x[x %in% 0:9] |>
        paste(collapse = '') |>
        as.numeric()
      
      name <- x[!cumsum(x %in% 0:9)] |>
        head(-1)
      real_name <- sapply(name, \(y) {
        if (y == '-') return(' ')
        letters[1 + (utf8ToInt(y) - 97 + id) %% 26]
      })
      real_name |>
        paste(collapse = '') |>
        paste(':', id)
    })
}

decrypted_names <- decrypt_rooms(input)
decrypted_names[grepl('north', decrypted_names)]
