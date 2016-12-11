#Gives the inputs and then
# the expected outputs for 
# each condition
input.1 <- c(1,1,0,0)
input.2 <- c(1,0,1,0)

xor.expected <- c(0,1,1,0)
nxor.expected <- c(1,0,0,1)

#numebr of tiemsteps each network is
# run for
timesteps <- 20

#sigmoid function for the final output
sigmoid.activation <- function(x){
  if(x == 0){
    x <- 10^-10
  }
  return(1 / (1+exp(-x)))
}

run.network <- function(indiv){
  #defines the parts of a network so 
  # they are easier to use
  weights <- indiv[[1]]
  operations <- indiv[[2]]
  inputs <- indiv[[3]]
  outputs <- indiv[[4]]
  
  n.nodes <- length(weights)
  n.edges <- length(inputs)
  
  #Creates places for storing node values
  current.value.nodes <- numeric(n.nodes + 1)
  new.value.nodes <- numeric(n.nodes + 1)
  
  #Sets the outputs in the case of null networks
  # otherwise they are overwritten
  if(xor){
    final.outputs <- nxor.expected
  }
  else{
    final.outputs <- xor.expected
  }
  
  #increases the value for the null networks if a network is
  # nonfunctional
  if(length(inputs) == 0 || length(outputs) == 0 || length(inputs) != length(outputs)){
    nulls[g] <<- nulls[g] + 1
  }
  
  #Only runs functional networks
  if(length(inputs) != 0 && length(outputs) != 0 && length(inputs) == length(outputs)){
    for(q in 1:4){ 
      #Sets inputs to the correct vlaeu of the 4
      in.1 <- input.1[q]
      in.2 <- input.2[q]
      for(t in 1:timesteps){
        #runs for each edge
        for(i in 1:n.edges){
          #If the input is input 1
          if(inputs[i] == "I1"){
            #If it's the output jsut give it the value
            if(outputs[i] == "O"){
              new.value.nodes[n.nodes + 1] <- new.value.nodes[n.nodes + 1] + in.1
            }
            #Otherwise you need a different way to index the node
            else{
              new.value.nodes[as.integer(outputs[i])] <- new.value.nodes[as.integer(outputs[i])] + in.1
            }
          }
          
          #If the input is input 2
          else if(inputs[i] == "I2"){
            #If it's the output jsut give it the value
            if(outputs[i] == "O"){
              new.value.nodes[n.nodes + 1] <- new.value.nodes[n.nodes + 1] + in.2
            }
            #Otherwise you need a different way to index the node
            else{
              new.value.nodes[as.integer(outputs[i])] <- new.value.nodes[as.integer(outputs[i])] + in.2
            }
          }
          
          #For non input nodes (0 is "+", 1 is "*")
          else{
            #Addition: The node increases it's value by the node that is inputting
            # to its value plus that node's weight
            if(operations[as.integer(inputs[i])] == 0){
              #Still need different indexing for the output vs the other nodes
              if(outputs[i] == "O"){
                new.value.nodes[n.nodes + 1] <- new.value.nodes[n.nodes + 1] + current.value.nodes[as.integer(inputs[i])] + weights[as.integer(inputs[i])]
              }
              else{
                new.value.nodes[as.integer(outputs[i])] <- new.value.nodes[as.integer(outputs[i])] + current.value.nodes[as.integer(inputs[i])] + weights[as.integer(inputs[i])]
              }
            #Multiplication: The node increases it's value by the node that is inputting
            # to its value times that node's weight
            }
            else if(operations[as.integer(inputs[i])] == 1){
              #Still need different indexing for the output vs the other nodes
              if(outputs[i] == "O"){
                new.value.nodes[n.nodes + 1] <- new.value.nodes[n.nodes + 1] + current.value.nodes[as.integer(inputs[i])] * weights[as.integer(inputs[i])]
              }
              else{
                new.value.nodes[as.integer(outputs[i])] <- new.value.nodes[as.integer(outputs[i])] + current.value.nodes[as.integer(inputs[i])] * weights[as.integer(inputs[i])]
              }
            }
          }
        }
        #sets all current values to now be equal to the 
        # new vlaues and resets the new valeus for the next
        # timestep
        for(i in 0:n.nodes + 1){
          current.value.nodes[i] <- new.value.nodes[i]
          new.value.nodes[i] <- 0
        }
      }
      #Runs the output nodes value through the sigmoid function
      #before storing it as the final output value
      final.outputs[q] <- sigmoid.activation(current.value.nodes[n.nodes + 1])
    }
  }
  return(final.outputs)
}


