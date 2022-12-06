a=readLines('5')
s=strsplit
y=sapply
y(1:2,\(v){
C=apply(y(s(a[grep('\\[',a)],''),\(x)x[seq(2,length(x),4)]),1,\(x)rev(x[x!=' ']))
for(p in lapply(s(a[grep('m',a)],' '),\(x)as.integer(x[c(2,4,6)]))){
C[[p[3]]]=c(C[[p[3]]],c(rev,c)[[v]](tail(C[[p[2]]],p[1])))
C[[p[2]]]=head(C[[p[2]]],-p[1])}
paste(y(C,tail,1),collapse='')})
