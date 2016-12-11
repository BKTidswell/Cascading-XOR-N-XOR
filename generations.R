#creates one network
make.individual <- function(id){
  #defiens a number of nodes and edges
  n.nodes <- round(rnorm(1,node.mean,node.sd))
  n.edges <- round(rnorm(1,edge.mean,edge.sd))
  
  #Sets lists to fill with values
  weights <- numeric(n.nodes)
  operations <- numeric(n.nodes)
  input.vertices <- numeric(n.nodes + 2)
  output.vertices <- numeric(n.nodes + 1)
  edge.inputs <- numeric(n.edges)
  edge.outputs <- numeric(n.edges)
  
  #Creates weights and operations for
  # each new node
  for (i in 1:n.nodes) {
    weights[i] <- runif(1,-10, 10)
    operations[i] <- sample(c(0,1), 1)
  }
 
  #Creates refernces for the input and
  # output nodes in the verticies list
  input.vertices[n.nodes + 1] <- "I1"
  input.vertices[n.nodes + 2] <- "I2"
  output.vertices[n.nodes + 1] <- "O"
  
  #fills verticies list with node numbers
  for(i in 1:n.nodes){
    input.vertices[i] <- i
    output.vertices[i] <- i
  }
  
  #Makes sure that each orgional network connects to an output
  #node
  while(!is.element("O",edge.outputs) || !is.element("I1",edge.inputs) 
                                      || !is.element("I2",edge.inputs)){
    for(i in 1:n.edges){
      #Makes an input output pair by sampling from the list 
      # of verticies
      edge.inputs[i] <- sample(input.vertices,1)
      edge.outputs[i] <- sample(output.vertices,1)
    }
  }
  
  #Conncatonates the weights, operations, inptus and outputs, and id 
  # into one indvidual network
  genome <- list(weights, operations, edge.inputs, edge.outputs,id)
  
  return(genome)
}

make.individual()

#This makes a whole generation by
# making a bunch of individuals
make.generation <- function(){
  generation <- list()
  for(i in 1:individuals){
    generation[[i]] <- make.individual(i)
  }
  return(generation)
}

make.generation()
