# initialize a bunch of values for c
# given the norm, its strength (t -> l), and the productiveness of the environment
# sample from the list of c's weighted by their fitness
# repeat sampling

# --> in a fixed world, with fixed norms, fixed environment, 
# what should people do, given the tightness of the norm

#start recording time
start.time = Sys.time()

par(mfrow=c(2,2),oma=c(2,2,4,2))

simulation = c(1, 10, 100, 1000)
for(s in simulation){
  n_sims = s
  gen = 1000
  end_result = as.data.frame(matrix(data = NA, nrow = n_sims, ncol = gen))
  for(sim in 1:n_sims){
    ## PUBLIC GOODS GAME ##
    # number of individuals
    n = 100
    # multiplier
    m = 1.5
    # baseline fitness for all individuals in first generation
    b = 15
    # contribution scale, 0 = no contribution, 9 = maximum possible amount of contribution
    c = 0:9
    # number of generations
    gen = 1000
    # mutation rate
    mu = 0.01
    
    # initialize a list that will be populated with the scores of each individual in each generation
    score = vector(mode = "list", length = gen)
    
    # initialize a list that will be populated with the probabilities (fitness) of the contributions in each generation
    fitness = vector(mode = "list", length = gen)
    
    # initialize a list that will be populated with the inherited contributions of each individual in each generation
    inherited_contribution = vector(mode = "list", length = gen)
    # sample inherited contributions for the first generation (all inherited contributions will have equal weights in the first generation)
    inherited_contribution[[1]] = sample(c, n, replace = TRUE)
    
    # some of the inherited contribution values will change due to mutation (social learning)
    # sample mutated contributions
    mutated_contribution = sample(c, n, replace = TRUE)
    # determine which of the individuals will have the mutated contribution (a vector of 0s and 1s)
    does_mutate = (runif(n, 0, 1)<mu)*1
    # initialize a list that will be populated with the actual contributions of each individual in each generation
    actual_contribution = vector(mode = "list", length = gen)
    # change the contribution value of those who choose to socially learn in the first generation (switch their inherited c to mutated c)
    actual_contribution[[1]] = mutated_contribution*does_mutate + inherited_contribution[[1]]*(1-does_mutate)
    
    #initialize a vector that will be populated with the payoff (amount everyone gets back) in each generation
    payoff = rep(NA, gen)
    # amount everyone gets back in the first generation
    payoff[1] = (m*sum(actual_contribution[[1]]))/n
    
    # fitness of each individual based on their contribution 
    # (those who contributed more will have a lower fitness since everyone got back the same amount, 
    # so contributing more is costly)
    score[[1]] = b - actual_contribution[[1]] + payoff[1]
    
    #from the fitness of individuals we could assign weights (probabilities) for each contribution choice
    fitness[[1]] = score[[1]]/sum(score[[1]])
    
    for(i in 2:gen){
      inherited_contribution[[i]] = sample(actual_contribution[[i-1]], n, replace = TRUE, fitness[[i-1]])
      ##mutation stage##
      mutated_contribution = sample(c, n, replace = TRUE)
      does_mutate = (runif(n, 0, 1)<mu)*1
      actual_contribution[[i]] = mutated_contribution*does_mutate + inherited_contribution[[i]]*(1-does_mutate)
      ##################
      payoff[[i]] = (m*sum(actual_contribution[[i]]))/n
      score[[i]] = b - actual_contribution[[i]] + payoff[i]
      fitness[[i]] = score[[i]]/sum(score[[i]])
    }
    
    actual_contribution = unlist(lapply(actual_contribution, mean))
    actual_contribution_normalized = actual_contribution/sum(actual_contribution)
    
    end_result[sim,] = actual_contribution_normalized
  }

  end_result = colMeans(end_result)
  plot(1:gen, end_result, type = "l", lwd = 1.25, 
       xlab = "Generation", ylab = "Fitness", main = paste(s, "simulation(s)"))
}

#add main title
mtext(expression(bold("Public goods game (without punishment)")), side = 3, line = 1, outer = TRUE, cex = 1.15, ps = 50)

#stop recording time
end.time = Sys.time()
time.taken = round(end.time - start.time,2)
time.taken