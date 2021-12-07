# --- Day 7: The Treachery of Whales ---

input <- scan('2021/input/input-07.txt', sep = ',')

# Part 1
min(sapply(min(input):max(input), \(x) sum(abs(x-input))))

# Part 2
min(sapply(min(input):max(input), \(x) sum(sapply(abs(x-input), \(y) sum(seq(y))))))
