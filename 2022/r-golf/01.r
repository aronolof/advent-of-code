x=as.numeric(readLines("1"))
max(sapply(split(x,cumsum(is.na(x))),sum,na.rm=T))
sum(sort(sapply(split(x,cumsum(is.na(x))),sum,na.rm=T),T)[1:3])
