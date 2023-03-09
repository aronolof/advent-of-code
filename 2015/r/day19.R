# --- Day 9: Explosives in Cyberspace ---
input <- readLines('2015/data/input19.txt')

replacements <- head(input, -2) |>
  strsplit(' => ')

molecule <- tail(input, 1)

make_molecules <- function(m, replacements) {

  lapply(replacements, \(r) {
    
    matches <- gregexec(r[1], m)
    match_length <- attributes(matches[[1]])$match.length
    
    if (match_length[[1]][1] == -1) return(NULL)
    
    lapply(seq_along(matches[[1]]),
           \(j) {
             paste0(
               substring(m, 0, matches[[1]][j] - 1),
               r[2],
               substring(m, matches[[1]][j] + match_length[j])
             )
           })
  }) |>
    unlist() |>
    unique()
}

# Part 1
make_molecules(molecule, replacements) |>
  length()

reverse_replacements <- lapply(replacements, \(x) x[2:1])
