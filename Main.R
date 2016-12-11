library(igraph)

#Intializes the list for the generation tree, as well as the initial
# popultion parameters
gen.records <- numeric()

node.mean <- 15
node.sd <- 2

edge.mean <- 75
edge.sd <- 4

individuals <- 100

xor <- TRUE
counter <- 0

#Creates the first generation
gen <- make.generation()
generations <- 350

#Creates the list to store data across generations in
nulls <- numeric(generations)
mean.fitness <- numeric(generations)
min.fitness <- numeric(generations)
max.fitness <- numeric(generations)
first.quartile <- numeric(generations)
third.quartile <- numeric(generations)
mean.edges <- numeric(generations)
mean.nodes <- numeric(generations)
mod <- numeric(generations)

#Makes progress bar
pb <- txtProgressBar(min=0, max=generations, style=3)

for(g in 1:generations){
  #Makes the list for fitness values
  fit.list <- list()
  
  #Gets the outputs for each network
  for (i in 1:individuals) {
    fit.list[[i]] <- run.network(gen[[i]])
  }
  
  #increments the switching counter, and if it is
  # above the given value then the fitness condition
  # switches
  counter <- counter + 1
  
  if(counter >= generations/4){
    counter <- 0
    xor <- !xor
  }
  
  #Sets the fitness scores to be fitness values from all the
  # outputs
  fit.scores <- fitness(fit.list)
  
  #Records values for the gereations
  mean.fitness[g] <- mean(fit.scores)
  max.fitness[g] <- max(fit.scores)
  min.fitness[g] <- min(fit.scores)
  first.quartile[g] <- quantile(fit.scores)[2]
  third.quartile[g] <- quantile(fit.scores)[4]
  
  for(i in 1:individuals) {
    mean.edges[g] <- mean.edges[g] + length(gen[[i]][[3]])
    mean.nodes[g] <- mean.nodes[g] + length(gen[[i]][[1]])
    mod[g] <- mod[g] + calc.mod(gen[[i]])
  }
  
  #Determines the parents based on their scores and the 
  # roulette wheel function
  parents <- which.repo(fit.scores)
  
  #Sets the new generation based on the 
  # parents and the prvious gerenation using
  # the mutation functions
  gen <- mute.genome(parents, gen)
  
  #increases progess bar
  setTxtProgressBar(pb, g)
}

#Calcualtes values for graphing
prop.edges <- (mean.edges/individuals)/max(mean.edges/individuals)
prop.nodes <- (mean.nodes/individuals)/max(mean.nodes/individuals)
mean.mod <- (mod/individuals)

#Graphs the desired values
plot(1:generations,mean.fitness,col = 'blue', type = 'l', ylim = c(0,1),ylab="Fitness", xlab = "Generation")
par(new = TRUE)
plot(1:generations,mean.mod,col = 'orange', type = 'l', ylim = c(0,1),ylab="Fitness", xlab = "Generation")
par(new = TRUE)
plot(1:generations,nulls/individuals,col = 'black', type = 'l', ylim = c(0,1),ylab="Fitness", xlab = "Generation")
#par(new = TRUE)
#plot(1:generations,prop.edges,col = 'firebrick1', type = 'l', ylim = c(0,1),ylab="Fitness")
#par(new = TRUE)
#plot(1:generations,prop.nodes,col = 'springgreen', type = 'l', ylim = c(0,1),ylab="Fitness")
#par(new = TRUE)
#plot(1:generations,first.quartile,col = 'purple', type = 'l', ylim = c(0,1),ylab="Fitness")
#par(new = TRUE)
#plot(1:generations,third.quartile,col = 'orange', type = 'l', ylim = c(0,1),ylab="Fitness")

#g <- graph(gen.records)
#plot(g, edge.color = "black", edge.arrow.width=0.05, edge.arrow.size=0.1, edge.width = 0.5, vertex.size=0.1, vertex.label=NA, vertex.color = "blue", layout = layout.reingold.tilford(g, root= c(1:individuals)))
