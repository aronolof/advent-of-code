x=utf8ToInt(readLines('6'))
s=sapply
s(c(4,14),\(y)which.max(s(seq(x),\(i)length(unique(x[tail(seq(i),y)]))==y)))
