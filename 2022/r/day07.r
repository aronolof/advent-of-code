# --- Day 7: No Space Left On Device ---
input <- tibble(x = readLines("2022/data/input07.txt")) %>%
  separate(x, into=c('a', 'b', 'c'), sep=' ', fill = 'right') %>%
  select(a, b, c) %>%
  mutate(path = NA)

pointer <- NULL
for(i in seq(nrow(input))) {
  if(input$b[i] == 'cd' & input$c[i] != '..') pointer <- c(pointer, input$c[i])
  if(input$b[i] == 'cd' & input$c[i] == '..') pointer <- head(pointer, -1)
  input$path[i] <- paste(pointer, collapse='/')
}
size = input %>%
  mutate(size = as.integer(str_extract(a, '[0-9]*'))) %>%
  mutate(size = pmax(size, 0, na.rm=TRUE)) %>%
  group_by(path) %>%
  summarise(folder_size = sum(size, na.rm=T))

size$nested_size <-
  sapply(size$path, \(x) {
    size %>% filter(str_detect(path, paste0('^', x, '.'))) %>% mutate(total=folder_size) %>% pull(total) %>% sum(na.rm=T)
  })

# Part 1
size %>%
  arrange(path) %>%
  mutate(total_size = folder_size + nested_size) %>%
  filter(total_size <= 100000) %>%
  arrange(path) %>%
  pull(total_size) %>% sum()

# Part 2
size %>%
  arrange(path) %>%
  mutate(total_size = folder_size + nested_size) %>%
  filter(total_size >= 30000000-(70000000-size$nested_size[1])) %>%
  pull(total_size) %>%
  min()

