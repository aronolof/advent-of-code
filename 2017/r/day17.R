# --- Day 17: Spinlock ---

input <- 367
state <- 0
insert_pos <- 1

i = 1
for (i in 1:2017) {
  insert_pos <- ((insert_pos + input) %% length(state)) + 1
  state = append(state, i, after = insert_pos)
}

state[((insert_pos + 1) %% length(state)) + 1]
