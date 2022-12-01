# --- Day 1: Calorie Counting ---
input = readlines("2022/data/input01.txt")

function per_elf_sums(input)
    sums = [0]
    for x in input
        x == "" ? push!(sums, 0) : sums[end] += parse(Int, x)
    end
    sums
end

sums = per_elf_sums(input)

println("Answer 1: ", maximum(sums))
println("Answer 2: ", sum(sort(sums, rev=true)[1:3]))
