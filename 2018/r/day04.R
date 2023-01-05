# --- Day 4: Repose Record ---
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

input <- readLines('2018/data/input04.txt')

# Part 1
df <- as_tibble(do.call(rbind, strsplit(gsub('\\[', '', input), '\\] ')))

asleep_guard_minutes <- df %>%
  mutate(datetime = as.POSIXct(V1),
         ID = as.numeric(str_extract(V2, '\\d+')),
         minute = minute(datetime)) %>%
  arrange(datetime) %>% 
  fill(ID) %>% 
  filter(str_detect(V2, 'wakes|falls')) %>%
  mutate(date = date(datetime)) %>% 
  right_join(expand(., date, minute = 0:59)) %>%
  arrange(date, minute) %>%
  fill(ID, V2) %>% 
  filter(V2 == 'falls asleep')

asleep_guard_minutes %>% 
  group_by(ID) %>%
  summarise(n = n(), mode = which.max(tabulate(minute))) %>% 
  arrange(desc(n)) %>% 
  head(1) %>% 
  mutate(mode * ID) 

# Part 2
asleep_guard_minutes %>%
  count(ID, minute) %>% 
  arrange(desc(n)) %>% 
  head(1) %>% 
  mutate(minute * ID) 
