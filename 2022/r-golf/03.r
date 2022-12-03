x=lapply(readLines("3"),\(y)(utf8ToInt(y)-38)%%58)
sum(sapply(x,\(y)Reduce(intersect,split(y,seq(y)<=length(y)/2))))
sum(sapply(split(x,(seq(length(x))-1)%/%3),\(y)Reduce(intersect,y)))
