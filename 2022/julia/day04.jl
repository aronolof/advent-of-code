# --- Day 4: Camp Cleanup ---

parse_input = function(input_path)
    input = split.(readlines(input_path), (['-',','],))
    input_int = [parse.(Int, x) for x in input]
    return [[col[i] for col in input_int] for i in 1:4]
end

part01 = function(input_path)
    a, b, c, d  = parse_input(input_path)
    sum((a .>= c) .& (b .<= d) .| (a .<= c) .& (b .>= d))
end

part02 = function(input_path)
    a, b, c, d  = parse_input(input_path)
    sum((b .>= c) .& (a .<= d))
end

println("Answer 1: ", part01("2022/data/input04.txt"))
println("Answer 2: ", part02("2022/data/input04.txt"))
