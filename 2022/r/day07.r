# --- Day 7: No Space Left On Device ---
library(tidyverse)

df <- read_delim('2022/data/input07.txt', delim = ' ',
                 col_names = c('file_size', 'x', 'y'),
                 col_types = 'icc') %>%
  mutate(full_path = NA)

folder_path <- NULL

for (i in 1:nrow(df)) {
  if (df$x[i] == 'cd' & df$y[i] != '..') folder_path <- c(folder_path, df$y[i])
  if (df$x[i] == 'cd' & df$y[i] == '..') folder_path <- head(folder_path, -1)
  df$full_path[i] <- paste(folder_path, collapse = '/')
}

df <- df %>%
  group_by(full_path) %>%
  summarise(files_size = sum(file_size, na.rm = T)) %>%
  mutate(nested_files_size = sapply(full_path, \(x) {
    filter(., grepl(paste0('^', x, '.'), full_path)) %>%
      summarise(sum(files_size)) %>%
      pull()
  })) %>%
  mutate(total_size = files_size + nested_files_size)

# Part 1
df %>%
  filter(total_size <= 100000) %>%
  summarise(sum(total_size))

# Part 2
df %>%
  filter(total_size >= max(total_size) - 40000000) %>%
  summarise(min(total_size))

