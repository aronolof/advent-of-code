# --- Day 4: Passport Processing ---

input <- readLines("2020/input/input-04.txt")

# Part 1
sum(sapply(split(input, cumsum(input == "")),
           function(x) {
             all(sapply(c("byr","iyr","eyr","hgt","hcl","ecl","pid"),
                        function(y) grepl(y, paste(x, collapse = ""))))
           }))

# Part 2
passport_list <- lapply(split(input, cumsum(input == "")),
                        function(x) unlist(strsplit(x, " ")))

sum(sapply(passport_list, function(x) {
  v <- sub("[a-z]{3}:(.*)", "\\1", x)
  names(v) <- sub("([a-z]{3}):.*", "\\1", x)
  
  as.numeric(v["byr"]) %in% 1920:2002 &
    as.numeric(v["iyr"]) %in% 2010:2020 &
    as.numeric(v["eyr"]) %in% 2020:2030 &
    grepl("^[0-9]{9}$", v["pid"]) &
    v["ecl"] %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth") &
    grepl("^#[0-9a-f]{6}$", v["hcl"]) &
    floor(prod(as.numeric(sub("^([0-9]*)(cm|in)$*", "\\1", v["hgt"])),
                   (1 + grepl("^[0-9]*(in)$", v["hgt"]) * 1.55))) %in% 150:193
  }),
  na.rm = TRUE)



