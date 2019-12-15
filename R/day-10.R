# --- Day 10: Monitoring Station ---

library(tidyverse)

asteroid_pairs <- scan("input/10.txt",  what = "character") %>% str_split("", simplify = TRUE) %>%
  {as_tibble(cbind(id = c(.), x=c(col(.)-1), y=c(row(.)-1)))} %>%
  filter(id == "#") %>%
  mutate(dummy = 1, id = row_number()) %>%
  full_join(rename_all(., ~ paste0(., "2")), c("dummy" = "dummy2")) %>%
  mutate_all(as.numeric) %>%
  mutate(angle = atan2(y - y2, x - x2)) %>%
  filter(!(x == x2 & y == y2))

# Part 1
asteroid_pairs %>%
  group_by(id) %>%
  summarise(n_detected = n_distinct(angle)) %>%
  filter(n_detected == max(n_detected))

# Part 2
asteroid_pairs %>%
  filter(id == 213) %>%
  mutate(dist = sqrt((x - x2) ^ 2 + (y - y2) ^ 2)) %>%
  mutate(angle = (angle + pi + pi / 2) %% (2 * pi)) %>%
  arrange(angle, dist) %>%
  group_by(angle) %>%
  mutate(angle_n = row_number()) %>%
  arrange(angle_n, angle) %>%
  ungroup() %>%
  mutate(row_number = row_number()) %>%
  filter(row_number == 200) %>% 
  {.$x2 * 100 + .$y2}
