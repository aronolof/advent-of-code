input_data = [int(x) for x in open('2019/input/input-01.txt', 'r').read().split()]
    
# Part 1
get_fuel = lambda x: int(x)//3-2
print(sum(list(map(get_fuel, input_data))))

# Part 2
fuelception = lambda fuel: 0 if fuel <= 0 else fuel + fuelception(get_fuel(fuel))
print(sum(list(map(fuelception, map(get_fuel, input_data)))))
