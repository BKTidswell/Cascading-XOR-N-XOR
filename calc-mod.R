library(igraph)

#List of colors for node groups
color.list <- c("blue", "red", "orange", "yellow", "forestgreen", "purple", "deeppink", "lawngreen", "goldenrod4", "khaki2", "lightcoral", "black", "white", "grey")

#turns the two lists of inputs and outputs 
# ino one edge list usable by igraph
make.edge.list <- function(indiv){
  inputs <- indiv[[3]]
  outputs <- indiv[[4]]
  len.edges <- length(inputs)
  edge.list <- numeric()
  for (i in 1:len.edges){
    edge.list <- c(edge.list, edge.char.to.int(inputs[i]), edge.char.to.int(outputs[i]))
  }
  return(edge.list)
}

#Turns the char type for the edges into 
# ints for use by igraph
edge.char.to.int <- function(edge){
  max.nodes <- 50
  ref.edges <- c(1:(max.nodes + 3)) - 3
  ref.edges[1] <- "O"
  ref.edges[2] <- "I1"
  ref.edges[3] <- "I2"
  return(match(edge, ref.edges))
}

#calcualtes the optimal cluster modularity of
# a graph
calc.mod <- function(indiv){
  edge.list <- make.edge.list(indiv)
  if(anyNA(edge.list)){
    return(1)
  }
  g <- graph(edge.list)
  mod <- cluster_optimal(g, weights = NULL)
  return(modularity(mod))
}

#plots a graph colroign it by its groups
plot.coms <- function(indiv){
  edge.list <- make.edge.list(indiv)
  g <- graph(edge.list)
  clp <- cluster_label_prop(g)
  V(g)$community <- clp$membership
  colrs <- adjustcolor(sample(color.list, length(clp)), alpha=0.8)
  plot(g, vertex.color=colrs[V(g)$community])
}

#Tester functions for modularity and 
# ploting networks
mod.list <- numeric(length(gen))
for(i in 1:length(gen)){
  mod.list[i] <- calc.mod(gen[[i]])
}

hist(mod.list)
plot.coms(gen[[which.max(mod.list)]])
plot.coms(gen[[which.min(mod.list)]])

max(mod.list)
