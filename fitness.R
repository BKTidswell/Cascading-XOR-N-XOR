#Sets the expected valeus for each condition
xor.expected <- c(0,1,1,0)
nxor.expected <- c(1,0,0,1)

#calcualtes the fitness valsues for a generation
fitness <- function(results){
  individuals <- length(results)
  fitness.results <- numeric(individuals)
  
  #Sets the expected value depending on the 
  #current condition
  if(xor){
    expected <- xor.expected
  }
  else{
    expected <- nxor.expected
  }
  
  #Gets the RMSE for each indvidual
  for (i in 1:individuals) {
      fitness.results[i] <- sqrt(mean((expected - results[[i]])^2)) 
  }
  
  #Resutls the result of exp(-RMSE)  so that those
  # with scores closer to 0 have higher fitness
  return(exp(-fitness.results))
}
