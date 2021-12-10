# --- Day 10: Syntax Scoring ---

input <- scan('2021/input/input-10.txt', what = character())

# Part 1
simplified_input <- input

while(T) {
  previous_input <- simplified_input
  simplified_input <- gsub('(\\[\\]|\\(\\)|\\{\\}|<>)', '', simplified_input)
  if(!any(nchar(previous_input) != nchar(simplified_input))) break
}

illegal <- substr(gsub('(\\(|\\{|\\[|<)', '', simplified_input), 1, 1)
scores <- c(`)`=3, `]`=57, `}`= 1197, `>`= 25137)
sum(scores[illegal], na.rm = TRUE)

# Part 2
valid_input <- simplified_input[is.na(scores[illegal])]

sapply(strsplit(valid_input, ''),
       \(x) {
         score <- 0
         for(i in rev(x)) {
           score <- score * 5 + c(`(`=1, `[`=2, `{`= 3, `<`= 4)[i]
         }
         score
       }
       ) |>
  median()
