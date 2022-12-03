# --- Day 3: Rucksack Reorganization ---

rucksacks = [mod.(x.-'&', 58) for x in collect.(readlines("2022/data/input03.txt"))] 

function part1(rucksacks)
    prio = []
    for rucksack in rucksacks
        overlap = intersect(rucksack[1:div(end, 2)], rucksack[1+div(end,2):end])[1]
        push!(prio, overlap)
    end
    return sum(prio)
end

function part2(rucksacks)
    prio = []
    for i in collect(1:3:length(rucksacks))
        overlap = intersect(rucksacks[i], rucksacks[i+1], rucksacks[i+2])[1]
        push!(prio, overlap)
    end
    return sum(prio)
end

println("Answer 1: ", part1(rucksacks))
println("Answer 2: ", part2(rucksacks))
