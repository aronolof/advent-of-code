# --- Day 4: Secure Container ---

library(tidyverse)

# Part 1
passwords <- tibble(input = 136760:595730) %>%
  mutate_at(1, ~str_pad(., width = 6, pad = "0")) %>%
  separate(1, paste(1:6), 1:6, remove = FALSE) %>%
  mutate_all(as.numeric) %>%
  filter(rowMeans(.[3:7] >= .[2:6]) == 1 & rowMeans(.[3:7] == .[2:6]) != 0)
nrow(passwords)

# Part 2
passwords %>%
  pivot_longer(-input) %>%
  count(input, value) %>%
  filter(n == 2) %>%
  count(input) %>%
  nrow()
