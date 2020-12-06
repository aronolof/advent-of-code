# --- Day 6: Universal Orbit Map ---

library(tidyverse)
input <- read_delim("2019/input/input-06.txt", delim = ")", col_names = paste(1:2))
df <- filter(input, `1` == "COM")

while(!all(is.na(df[[ncol(df)]]))) {
  df <- left_join(df, setNames(input, ncol(df) + 0:1))
}

# Part 1
df %>%
  pivot_longer(-1) %>%
  filter(!is.na(value), !duplicated(value)) %>%
  summarise(sum(as.numeric(name) - 1))

# Part 2
df %>%
  filter_all(any_vars(. == "YOU" | . == "SAN")) %>% 
  {as_tibble(t(.))} %>%
  filter(xor(is.na(V1), is.na(V2)) | V1 != V2) %>%
  summarise(sum(!is.na(.)) - 2)