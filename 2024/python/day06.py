with open("2024/data/input06.txt", "r") as file:
    m = []
    for line in file:
        m.append(list(line)[:-1])

for i in range(len(m)):
    for j in range(len(m[0])):
        if m[i][j] == "^":
            px = i
            py = j
            dx = -1
            dy = 0
            m[i][j] = "X"
            break


visits = 1
while True:
    if px + dx < 0 or px + dx > len(m) - 1 or py + dy < 0 or py + dy > len(m[0]) - 1:
        break
    next_loc = m[px + dx][py + dy]
    if next_loc in [".", "X"]:
        if next_loc == ".":
            m[px + dx][py + dy] = "X"
            visits += 1
        px += dx
        py += dy
    elif next_loc == "#":
        temp = dx
        dx = dy
        dy = -temp
print(visits)
