# --- Day 2: Rock Paper Scissors ---

using DelimitedFiles

input = readdlm("2022/data/input02.txt", ' ', Char)

function part1(input)
    score = 0
    for row in eachrow(input)
        opponent = row[1] - 'a'
        you = mod(row[2] - 'a', 3) + 1

        score += [3, 6, 0][1 + mod(you - opponent, 3)] + you
        
    end
    return score
end

println("Answer 1: ", part1(input))

function part2(input)
    score = 0
    for row in eachrow(input)
        opponent = row[1] - 'a'
        you = mod(row[2] - 'a', 3) + 1

        score += [0, 3, 6][you] + (1 + mod(opponent + you, 3))
    end
    return score
end

println("Answer 2: ", part2(input))
