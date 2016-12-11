#mutation types
#jiggleRandom = 1
#moveEdge = 2
#addEdge = 3
#removeEdge = 4
#addNode = 5
#removeNode = 6
mutation.chances <- c(75,13,4,4,4,4)

#chooses who reporduces based on a sample with each 
# individual's fitness score as their probability
which.repo <- function(fit.scores){
  gen.scores <- fit.scores/sum(fit.scores)

  parents <- sample(1:individuals,individuals,TRUE,gen.scores)
  
  return(parents)
}

#Preforms one mutation on each parent to make
# their children
mute.genome <- function(to.mute, generation){
  #makes a list for the new generation
  new.gen <- list()
  for (i in 1:individuals){
      #randomly selects one of the mutations with their
      # chance as the proportion value
      mute.type <- sample(1:6,1,TRUE,mutation.chances)
      if(mute.type == 1){
        new.indiv <- jiggle.random.node(generation[[to.mute[i]]])
      }
      else if(mute.type == 2){
        new.indiv <- move.edge(generation[[to.mute[i]]])
      }
      else if(mute.type == 3){
        new.indiv <- add.edge(generation[[to.mute[i]]])
      }
      else if(mute.type == 4){
        new.indiv <- remove.edge(generation[[to.mute[i]]])
      }
      else if(mute.type == 5){
        new.indiv <- add.node(generation[[to.mute[i]]])
      }
      else if(mute.type == 6){
        new.indiv <- remove.node(generation[[to.mute[i]]])
      }
      #makes a new ID for the new indvidual
      new.indiv[[5]] <- i + individuals*g
      #Records the parent's ID and individual's ID for 
      # the generational tree
      gen.records <<- c(gen.records, generation[[to.mute[[i]]]][[5]], new.indiv[[5]])
      #Stores the new indivudual
      new.gen[[i]] <- new.indiv
  }
  return(new.gen)
}

#Changes one weight randomly by a value from -2 to 2
jiggle.random.node <- function(indiv){
  n.weights <- length(indiv[[1]])
  change.weight <- sample(1:n.weights, 1)
  indiv[[1]][change.weight] <- indiv[[1]][change.weight] + runif(1, -2, 2)
  return(indiv)
}

#Moves one edge randomly
move.edge <- function(indiv){
  n.nodes <- length(indiv[[1]])
  n.edges <- length(indiv[[3]])
  which.edge <- sample(1:n.edges, 1)
  new.input.list <- c(c(c(1:n.nodes), "I1"), "I2")
  new.output.list <- c(c(1:n.nodes), "O")
  IO <- sample(1:2,1)
  #1 = input change, 2 = output change
  if(IO == 1){
    new.edge <- sample(new.input.list,1)
    indiv[[3]][which.edge] <- new.edge
  }
  else if(IO == 2){
    new.edge <- sample(new.output.list,1)
    indiv[[4]][which.edge] <- new.edge
  }
  return(indiv)
}

#Adds an edge between two randomly selected 
#nodes
add.edge <- function(indiv){
  n.nodes <- length(indiv[[1]])
  n.edges <- length(indiv[[3]])
  new.input.list <- c(c(c(1:n.nodes), "I1"), "I2")
  new.output.list <- c(c(1:n.nodes), "O")
  new.input <- sample(new.input.list,1)
  new.output <- sample(new.output.list,1)
  indiv[[3]] <- c(indiv[[3]], new.input) 
  indiv[[4]] <- c(indiv[[4]], new.output) 
  return(indiv)
}

#Removes one edge
remove.edge <- function(indiv){
  n.edges <- length(indiv[[3]])
  to.remove <- sample(1:n.edges,1)
  indiv[[3]] <- indiv[[3]][-to.remove]
  indiv[[4]] <- indiv[[4]][-to.remove]
  return(indiv)
}

#Adds a new node with a new weight and
# operation
add.node <- function(indiv){
  new.weight <- runif(1,-10, 10)
  new.operation <- sample(0:1, 1)
  indiv[[1]] <- c(indiv[[1]], new.weight) 
  indiv[[2]] <- c(indiv[[2]], new.operation)
  return(indiv)
}

#More complicated: But does remove
# one node and all edges on it
remove.node <- function(indiv){
  n.nodes <- length(indiv[[1]])
  node.to.remove <- sample(1:n.nodes,1)
  indiv[[1]] <- indiv[[1]][-node.to.remove]
  indiv[[2]] <- indiv[[2]][-node.to.remove]
  #Clears all inputs and outputs that connect to that node
  inputs.to.clear <- which(indiv[[3]] == node.to.remove)
  outputs.to.clear <- which(indiv[[4]] == node.to.remove)
  all.to.clear <- c(inputs.to.clear,outputs.to.clear)
  #If some edges are to be cleared they are cleared
  if(length(all.to.clear) != 0){
    indiv[[3]] <- indiv[[3]][-all.to.clear]
    indiv[[4]] <- indiv[[4]][-all.to.clear]
  }
  #This changes the value of edges that connected to nodes with a higher
  # index vlaue than the removed node so that they still connect to
  # the node they were meant to and don't casue tons of off-by-one
  # errors
  if(length(indiv[[3]]) != 0 && length(indiv[[4]]) != 0 && length(indiv[[3]]) == length(indiv[[4]])){
     for(i in 1:length(indiv[[3]])){
        if(indiv[[3]][i] != "I1" && indiv[[3]][i] != "I2"){
          if(as.integer(indiv[[3]][i]) > node.to.remove){
            indiv[[3]][i] <- as.integer(indiv[[3]][i]) - 1
          }
        }
        if(indiv[[4]][i] != "O"){
          if(as.integer(indiv[[4]][i]) > node.to.remove){
            indiv[[4]][i] <- as.integer(indiv[[4]][i]) - 1
          }
        }
     }
  }
  return(indiv)
}


