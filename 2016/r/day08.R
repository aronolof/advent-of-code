# --- Day 8: Two-Factor Authentication ---
input <- readLines('2016/data/input08.txt')

# Part 1
instructions <- input |>
  strsplit(' |=')

lights <- c()
for (inst in instructions) {
  if (inst[1] == 'rect') {
    dims <- as.numeric(strsplit(inst[2], 'x')[[1]])
    x <- rep(seq(dims[1]) - 1, each = dims[2])
    y <- rep(seq(dims[2]) - 1, times = dims[1])
    lights <- union(lights, complex(real = x, imaginary = y))
  } else if (inst[2] == 'column') {
    lights[Re(lights) == as.numeric(inst[4])] <- lights[Re(lights) == as.numeric(inst[4])] + complex(real = 0, imaginary = as.numeric(inst[6]))
  } else if (inst[2] == 'row') {
    lights[Im(lights) == as.numeric(inst[4])] <- lights[Im(lights) == as.numeric(inst[4])] + as.numeric(inst[6])
  }
  lights <- unique(complex(real = Re(lights) %% 50, imaginary = Im(lights) %% 6))
}

length(lights)

# Part 2
plot(Re(lights), -Im(lights), pch = 15, asp = 1)
