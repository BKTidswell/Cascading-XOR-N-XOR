source('generations.R')
source('run-network.R')
source('fitness.R')
source('new-generation.R')
#library(jsonlite)

gen <- make.generation()
gen[[1]]
generations <- 100
mean.fitness <- numeric(generations)
#pb <- txtProgressBar(min=0, max=generations, style=3)

for(i in 1:generations){
  fit.list <- list()
  for (i in 1:10) {
    fit.list[[i]] <- run.network(gen[[i]])
  }

  fit.scores <- fitness.xor(fit.list)
  mean.fitness <- mean(fit.scores)
  
  parents <- which.repo(fit.scores)

  gen <- mute.genome(parents, gen)
  #setTxtProgressBar(pb, i)
}

plot(fit.scores)
