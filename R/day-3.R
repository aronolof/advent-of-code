# --- Day 3: Crossed Wires ---

library(dplyr)
library(tidyr)
library(readr)

step_coord <- function(x, dirs = c("L", "R")) {
  dir = substr(x, 1, 1)
  ((dir %in% dirs) * ((-1) ^ (dir == dirs[1])) * as.numeric(substr(x, 2, nchar(x)))) %>% cumsum()
}

wire_expand <- function(df, column) {
  
  df %>%
    mutate_at(column, list(
      x = ~ step_coord(., dirs = c("L", "R")),
      y = ~ step_coord(., dirs = c("D", "U"))
    )) %>%
    mutate(diff = replace_na(lead(x) - x, 1)) %>%
    uncount(pmax(abs(diff), 1), .id = "id") %>%
    mutate(x = x + (id - 1) * (-1) ^ (diff < 0)) %>%
    mutate(diff = replace_na(lead(y) - y, 1)) %>%
    uncount(pmax(abs(diff), 1), .id = "id") %>%
    mutate(y = y + (id - 1) * (-1) ^ (diff < 0)) %>%
    select(x, y)
}

input <- read_delim("input/input-day-3.txt",
                           delim = ",",
                           col_names = FALSE) %>%
  t() %>%
  as_tibble() %>%
  setNames(paste0("w", 1:2))

# Part 1
inner_join(
  wire_expand(input, "w1"),
  wire_expand(input, "w2")
) %>%
  mutate(dist = abs(x) + abs(y)) %>%
  arrange(dist) %>%
  head(1)

# Part 2
inner_join(
  mutate(wire_expand(input, "w1"),
         steps_w1 = row_number() + abs(first(x)) + abs(first(y)) - 1), 
  mutate(wire_expand(input, "w2"),
         steps_w2 = row_number() + abs(first(x)) + abs(first(y)) - 1)
) %>%
  mutate(total_steps = steps_w1 + steps_w2) %>%
  arrange(total_steps) %>%
  head(1)




# Bonus chart!

library(ggplot2)

input %>%
  mutate_at(c("w1", "w2"), list(
    x = ~ step_coord(., dirs = c("L", "R")),
    y = ~ step_coord(., dirs = c("D", "U"))
  )) %>%
  ggplot() +
  geom_path(aes(x = w1_x, y = w1_y), col = "#fffa65", size = 1) +
  geom_path(aes(x = w2_x, y = w2_y), col = "#cd84f1", size = 1) +
  geom_point(data = inner_join(wire_expand(input, "w1"), wire_expand(input, "w2")),
    mapping = aes(x = x, y = y),
    colour = "#ff4d4d",
    size = 3
  ) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"))
  
ggsave("R/day-3-bonus-chart.png", height = 10, width = 10)

