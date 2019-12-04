# --- Day 4: Secure Container ---

library(dplyr)
library(stringr)
library(tidyr)

input <- scan("input/input-day-4.txt", sep = "-")

# Part 1:
passwords <- tibble(input = input[1]:input[2]) %>%
  mutate(input_string = str_pad(as.character(input), width = 6, pad = "0")) %>%
  separate(input_string, sep = c(1:6), into = paste0("d", 1:6)) %>%
  mutate_all(as.numeric) %>%
  filter(d2>=d1, d3>=d2, d4>=d3, d5>=d4, d6>=d5) %>%
  filter(d2==d1 | d3==d2 | d4==d3 | d5 == d4 | d6==d5)
nrow(passwords)

# Part 2:
passwords %>%
  pivot_longer(col = d1:d6) %>%
  count(input, value) %>%
  filter(n == 2) %>%
  count(input) %>%
  nrow()