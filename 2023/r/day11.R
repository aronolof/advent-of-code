# --- Day 11: Cosmic Expansion ---
input <- readLines("2023/data/input11.txt")

# Part 1
space <- do.call(rbind, strsplit(input, ''))
exp_rows <- which(rowSums(space != '.') == 0)
exp_cols <- which(colSums(space != '.') == 0)

galaxy_pairs <- which(space == '#', arr.ind = TRUE) |>
  (\(x) cbind(x, id = seq(nrow(x))))() |>
  (\(x) merge(x, x, by = NULL))() |>
  (\(x) x[x['id.x'] > x['id.y'],])()

result <- apply(galaxy_pairs, 1, \(x) {

    all_steps <- unname(abs(x['row.x'] - x['row.y']) + abs(x['col.x'] - x['col.y']))
    
    expanded_steps <- length(c(exp_rows[exp_rows %in% x['row.x']:x['row.y']],
                               exp_cols[exp_cols %in% x['col.x']:x['col.y']]))
    
    c(all_steps - expanded_steps, expanded_steps)
    
  })

sum(result[1,] + result[2,] * 2)

# Part 2
sum(result[1,] + result[2,] * 1000000)

