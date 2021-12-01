# --- Day 7: Handy Haversacks ---
input <- readLines("2020/input/input-07.txt")
bag_rules <- strsplit(input, "( bags contain |(?<=[0-9]) | bags?,?\\.? ?|no other)", perl=TRUE)

# Part 1
list_bags <- function(x) {
  which_match <- sapply(bag_rules, function(k) x %in% k[-1])
  get_matches <- sapply(bag_rules[which_match], head, 1)
  
  if( length(get_matches) == 0) {
    x
  } else {
    list(x, sapply(get_matches, list_bags))
  }
}

length(unique(unlist(list_bags("shiny gold"))))-1

# Part 2
count_bags <- function(x) {
  rule <- bag_rules[sapply(bag_rules, function(k) x == k[1])][[1]]
  bag_names <- rule[seq(3, length(rule), 2)]
  bag_count <- as.numeric(rule[seq(2, length(rule), 2)])
  
  if(bag_names[1] == "") {
    0
  } else {
    sum(bag_count + bag_count * unlist(sapply(bag_names, count_bags)))
  }
}

count_bags("shiny gold")
