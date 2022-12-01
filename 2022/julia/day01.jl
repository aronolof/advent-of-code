# --- Day 1: Calorie Counting ---

input = readlines("2022/data/input01.txt")

function part1(input)
    sums = Vector{Int}([0])
    for x in input
        if x == ""
            push!(sums, 0)
        else
            sums[end] += parse(Int, x)
        end
    end
    return(maximum(sums))
end

println("Answer 1: ", part1(input))

function part2(input)
    sums = Vector{Int}([0])
    for x in input
        if x == ""
            push!(sums, 0)
        else
            sums[end] += parse(Int, x)
        end
    end
    return(sum(sort(sums, rev=true)[1:3]))
end

println("Answer 2: ", part2(input))
