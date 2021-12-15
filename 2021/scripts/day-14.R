# --- Day 14: Extended Polymerization ---

input <- readLines("2021/input/input-14.txt")

# Step 1
dict <- setNames(gsub('(.)(.).*(.)$', '\\1\\3\\2', input[-(1:2)]), gsub('^(..).*', '\\1', input[-(1:2)]))

polymer <- input[1]

for(i in 1:10) {
  pairs <- paste(strsplit(polymer, '')[[1]],
                 c(strsplit(polymer, '')[[1]][-1], ''),
                 sep = '')
  
  pairs[!is.na(dict[pairs])] <- dict[pairs][!is.na(dict[pairs])]
  
  polymer <- paste(substr(pairs, 1, 2), collapse = '')
}

diff(range(table(strsplit(polymer, ''))))
