x=read.table(text=gsub("-|,"," ",readLines("4")))
sapply(1:2,\(i)sum((x[i]>=x[3]&x[2]<=x[4])|(x[1]<=x[2+i]&x[2]>=x[4])))
