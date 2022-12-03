x=read.table("2",sep=" ")
y=sapply(1:2,\(i)sapply(x[[i]],utf8ToInt)-c(64,87)[i])
sum(c(3,6,0)[1+(y[,2]-y[,1])%%3]+y[,2])
sum(c(0,3,6)[y[,2]]+(1+(y[,1]+y[,2])%%3))