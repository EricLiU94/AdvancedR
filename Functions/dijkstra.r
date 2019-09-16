wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

nod<- as.matrix(wiki_graph[1])
lank<- as.matrix(wiki_graph[2])
vikt <- as.matrix( wiki_graph[3])

# the total number of nodes
l<- max(nod)

temp_matris<- matrix(l*l ,l, l)*0

# to assign the connections to each matrix element 
for (i in nod) {
  temp1<-nod[i]
  temp2<-lank[i]
  temp3<-vikt[i]
  return(c(temp1, temp2, temp3))
  
  temp_matris[temp1, temp2]<- temp3
}