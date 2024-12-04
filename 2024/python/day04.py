with open("2024/data/input04.txt", "r") as file:
    m = []
    for line in file:
        m.append(list(line)[:-1])

# Part 1
search = "XMAS"


def check_letter(m, i, j, k, di, dj):
    if i in range(len(m)) and j in range(len(m[0])):
        if m[i][j] == search[k]:
            if len(search) - 1 == k:
                return 1
            else:
                return check_letter(m, i + di, j + dj, k + 1, di, dj)
        else:
            return 0
    else:
        return 0


n_matches = 0
for i in range(len(m)):
    for j in range(len(m[0])):
        n_matches += check_letter(m, i, j, k=0, di=0, dj=1)
        n_matches += check_letter(m, i, j, k=0, di=0, dj=-1)
        n_matches += check_letter(m, i, j, k=0, di=1, dj=0)
        n_matches += check_letter(m, i, j, k=0, di=-1, dj=0)
        n_matches += check_letter(m, i, j, k=0, di=1, dj=1)
        n_matches += check_letter(m, i, j, k=0, di=-1, dj=-1)
        n_matches += check_letter(m, i, j, k=0, di=1, dj=-1)
        n_matches += check_letter(m, i, j, k=0, di=-1, dj=1)
print(n_matches)

# Part 2
n_matches = 0
for i in range(1, len(m) - 1):
    for j in range(1, len(m[0]) - 1):
        if m[i][j] == "A":
            n = [m[i - 1][j - 1], m[i + 1][j + 1], m[i - 1][j + 1], m[i + 1][j - 1]]
            if n.count("M") == 2 and n.count("S") == 2 and n[0:2].count("S") == 1:
                n_matches += 1
print(n_matches)
