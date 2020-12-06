input_data = [int(x) for x in open('2019/input/input-02.txt', 'r').read().split(',')]

# part 1:
def runProgram(code, replace1, replace2):
    code[1:3] = [replace1, replace2]

    for i in range(0, len(code), 4):
        if code[i] == 1:
            code[code[i+3]] = code[code[i+1]] + code[code[i+2]]
        elif input_data[i] == 2:
            code[code[i+3]] = code[code[i+1]] * code[code[i+2]]
        elif input_data[i] == 99:
            return(code[0])

runProgram(input_data.copy(), 12, 2)

# part 2
for noun in range(0, 100):
    for verb in range(0, 100):
        if runProgram(input_data.copy(), noun, verb) == 19690720:
            print(100*noun+verb)
