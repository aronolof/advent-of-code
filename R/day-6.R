library(tidyverse)

df <- input <- read_delim("input/input-day-6.txt", delim = ")", col_names = paste(1:2))

while(any(!is.na(df[ncol(df)]))) {
  df <- left_join(df, setNames(input, ncol(df):(ncol(df)+1)))
}

# Solution 1
df %>%
  filter_at(1, all_vars(.=="COM")) %>%
  pivot_longer(-1) %>%
  filter_at("value", ~!is.na(.)) %>%
  distinct() %>%
  summarise(answer_1=sum(as.numeric(name))-nrow(.))

# Solution 2
df %>%
  filter_at(1, all_vars(.=="COM")) %>%
  filter_all(any_vars(.=="YOU" | .=="SAN")) %>%
  t() %>%
  as_tibble() %>%
  filter(xor(is.na(V1), is.na(V2)) | V1 != V2) %>%
  summarise(answer_2 = sum(!is.na(.)))-2


