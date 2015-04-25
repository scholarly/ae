baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"

getdata = function(local){
  if(!file.exists(local)){
    library(downloader)
    remote = paste0(baseurl,local)
    download(remote,local)
  }
  data = read.csv(local)#,stringsAsFactors=FALSE)
}

extract_features = function(data){
  data
}


users = getdata("users.csv")
edges = getdata("edges.csv")

#2.1
library(igraph)
graph = graph.data.frame(edges,FALSE,users)

#1.1
nrow(users)
mean(degree(graph))

#1.2
names(which.max(table(users$locale)))

#1.3
table(users$gender,users$school)

#2.2
plot(graph, vertex.size=5, vertex.label=NA)

c = clusters(graph)
sum(c$csize>1)
sum(c$csize==1)

#2.3
dd = degree(graph)
sum(dd>=10)

#2.4
V(graph)$size = dd/2+2
plot(graph, vertex.label=NA)
summary(V(graph)$size)

#3.1
V(graph)$color = "black"
V(graph)$color[V(graph)$gender == "A"] = "red"
V(graph)$color[V(graph)$gender == "B"] = "gray"
plot(graph, vertex.label=NA)
table(V(graph)$size,V(graph)$color)


#3.2
str(graph)
users[V(graph)$school=="AB",]

V(graph)$color = "grey"
V(graph)$color[V(graph)$school == "A"] = "red"
V(graph)$color[V(graph)$school == "B"] = "yellow"
V(graph)$color[V(graph)$school == "AB"] = "blue"
plot(graph, vertex.label=NA)

users[dd>=10,]$school

#3.3
V(graph)$color = "grey"
V(graph)$color[V(graph)$locale == "A"] = "red"
V(graph)$color[V(graph)$locale == "B"] = "yellow"
plot(graph, vertex.label=NA)

library(rgl)
coords <- layout.fruchterman.reingold(graph, dim=3)
rglplot(graph,vertex.label=NA,layout=coords)


g <- graph.lattice( c(3,3,3) )
coords <- layout.fruchterman.reingold(g, dim=3)
rglplot(g, layout=coords)
