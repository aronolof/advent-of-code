# --- Day 8: Space Image Format ---

library(tidyverse)

# Part 1
tibble(input = as.numeric(str_split(scan("input/8.txt",  what = "character"), "", simplify = TRUE))) %>%
  group_by(layer = (row_number() - 1) %/% 150) %>% 
  count(input) %>%
  pivot_wider(names_from = input, values_from = n) %>%
  ungroup() %>%
  filter(`0` == min(`0`)) %>%
  {prod(.$`1`, .$`2`)}

# Part 2
tibble(input = as.numeric(str_split(scan("input/8.txt",  what = "character"), "", simplify = TRUE))) %>%
  group_by(n = (row_number() - 1) %% 150) %>%
  filter(input != 2) %>%
  summarise(input = first(input)) %>%
  mutate(x = n %% 25, y = n %/% 25) %>%
  ggplot(aes(x = x, y = -y, fill = input)) +
  geom_tile()

