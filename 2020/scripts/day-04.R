# --- Day 4: Passport Processing ---

input <- readLines("2020/input/input-04.txt")

# Part 1
sum(sapply(split(input, cumsum(input == "")),
           function(x) {
             all(sapply(c("byr","iyr","eyr","hgt","hcl","ecl","pid"),
                        function(y) grepl(y, paste(x, collapse = ""))))
           }))

# Part 2
passport_list <- lapply(split(input, cumsum(input == "")), function(x) unlist(strsplit(x, " ")))

sum(sapply(passport_list, function(x) {
  v <- sub("[a-z]{3}:(.*)", "\\1", x)
  names(v) <- sub("([a-z]{3}):.*", "\\1", x)
  
  abs(as.numeric(v["byr"]) - 1920) <= 82 &
    abs(as.numeric(v["iyr"]) - 2010) <= 10 &
    abs(as.numeric(v["eyr"]) - 2020) <= 10 &
    grepl("^[0-9]{9}$", v["pid"]) &
    v["ecl"] %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth") &
    grepl("^#[0-9a-f]{6}$", v["hcl"]) &
    abs(floor(prod(as.numeric(sub("^([0-9]*)(cm|in)$*", "\\1", v["hgt"])),
                   (1 + grepl("^[0-9]*(in)$", v["hgt"]) * 1.55))) - 150) <= 43
  }),
  na.rm = TRUE)



