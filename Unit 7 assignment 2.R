edges <- read.csv("edges.csv")
users <- read.csv("users.csv")

(nrow(edges)*2)/nrow(users)
table(users$school, users$locale)
table(users$school, users$gender)

install.packages("igraph")
library(igraph)


?graph.data.frame
g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)

t <- degree(g)
table(t > 9)
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
18/2 +2
0/2 +2

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school== "AB"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale== "B"] = "gray"
plot(g, vertex.label=NA)

?igraph.plotting 
