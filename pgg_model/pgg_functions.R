# Functions used in the public goods game model

# Description:
# this function runs the public goods game model
# 
# Arguments:
# - n is the number of individuals in the population
# - m is the value of the multiplier
# - b is the value of the baseline (score everyone starts with in the beginning)
# - gen is the number of generations
# - mu is the value of the mutation rate
# - init_val is the initial starting contribution value of all individuals in the population
# - isCont is a boolean variable; if TRUE, contributions are continuous, if FALSE, they are discrete
# - isAdditive is a boolean variable; if TRUE, penalty is additive, if FALSE, it is multiplicative
# - c_min is the minimum threshold value of the contribution norm. If c_min = NULL, contribution norm is the average contribution in the population
# - norm_intensity is the value of the intensity of the norm (t->l)
# - n_sim is the number of simulations
#
# Return:
# a vector of length n (number of individuals) that contains the average contribution of the population in each generation
pgg_model = function(n = 100, m = 1.5, b = 15, gen = 1000, mu = 0.01, init_val = b,
                     isCont = TRUE, isAdditive = TRUE, c_min = 6, norm_intensity = 3, n_sim = 100){
  end_result = as.data.frame(matrix(data = NA, nrow = n_sim, ncol = gen))
  for(sim in 1:n_sim){
    
    # initialize a list that will be populated with the inherited contributions of each individual in each generation
    inherited_contribution = vector(mode = "list", length = gen)
    # initialize a list that will be populated with the actual contributions (i.e. after mutation) of each individual in each generation
    actual_contribution = vector(mode = "list", length = gen)
    # initialize a list that will be populated with the probabilities (fitness) of the contributions in each generation
    fitness = vector(mode = "list", length = gen)
    
    for (i in 1:gen) {
      # determine the inherited contributions, first generation is a special case, later generations depend on if the contribution is discrete or continuous
      if (i == 1) {
        inherited_contribution[[i]] = rep(init_val, n)
      } else {
        inherited_contribution[[i]] = inherited_c(actual_contribution[[i-1]], n, fitness[[i-1]])
      }
      # some of the inherited contribution values will change due to mutation (social learning)
      # actual contributions for the first generation after mutants (symmetric) are introduced
      actual_contribution[[i]] = mutation_sym(inherited_contribution[[i]], isCont, n, mu, b)
      # determine if the contribution norm is a minimum threshold or the average contribution in that generation
      c_norm = norm(c_min, actual_contribution[[i]])
      # fitness of each individual
      fitness[[i]] = f(actual_contribution[[i]], b, m, n, c_norm, norm_intensity, isAdditive)
    }
    
    actual_contribution = unlist(lapply(actual_contribution, mean))
    end_result[sim,] = actual_contribution
  }
  
  end_result = as.numeric(colMeans(end_result))
  return(end_result)
}

# Description:
# this function determines the type of the contribution norm (average vs minimum threshold)
#
# Arguments:
# - contribution is a vector of length n (number of individuals in the population) containing the contribution of each individual in the population
# - c_min is the minimum threshold value of the contribution norm. If c_min = NULL, contribution norm is the average contribution in the population
#
# Return:
# a numeric value that corresponds to the contribution norm in the population (either average or minimum threshold)
norm = function(c_min, contribution){
  if(length(c_min)){
    return(c_min)
  } else{
    return(mean(contribution))
  }
}

# Description:
# this function samples contributions for individuals in the new generation based on the contributions of the previous generation and their relative fitness
#
# Arguments:
# - parent_contribution is a vector of length n (number of individuals in the population) containing the contribution of each individual in the previous generation
# - n is the number of individuals in the population
# - parent_fitness is a vector of length n (number of individuals in the population) containing the fitness of each individual in the previous generation
#
# Return:
# a vector of length n (number of individuals) that contains the contributions of each individual in the new generation
inherited_c = function(parent_contribution, n, parent_fitness = NULL){
  return(sample(parent_contribution, n, replace = TRUE, parent_fitness))
}

# Description:
# this function calculates the contribution of each individual in the population after mutation is applied
#
# Arguments:
# - old_contribution is a vector of length n (number of individuals in the population) containing the contribution of each individual in the population before mutation is applied
# - c is a vector containing the possible contribution values (trait options) in the population
# - n is the number of individuals in the population
# - mu is the value of the mutation rate
#
# Return:
# a vector of length n (number of individuals) that contains the contributions of each individual in the population after mutation is applied
mutation = function(old_contribution, c, n, mu){
  # sample mutated contributions
  mutated_contribution = sample(c, n, replace = TRUE)
  # determine which of the individuals will have the mutated contribution (a vector of 0s and 1s)
  does_mutate = (runif(n, 0, 1)<mu)*1
  # change the contribution value of those who choose to socially learn (switch their inherited c to mutated c)
  new_contribution = mutated_contribution*does_mutate + old_contribution*(1-does_mutate)
  return(new_contribution)
}

# Description:
# this function calculates the contribution of each individual in the population after symmetric mutation is applied
#
# Arguments:
# - old_contribution is a vector of length n (number of individuals in the population) containing the contribution of each individual in the population before mutation is applied
# - isCont is a boolean variable; if TRUE, contributions are continuous, if FALSE, they are discrete
# - n is the number of individuals in the population
# - mu is the value of the mutation rate
# - b is the baseline value everyone in the population starts with
#
# Return:
# a vector of length n (number of individuals) that contains the contributions of each individual in the population after symmetric mutation is applied
mutation_sym = function(old_contribution,isCont, n, mu, b){
  # determine which of the individuals will have the mutated contribution (a vector of 0s and 1s)
  does_mutate = (runif(n, 0, 1)<mu)*1
  if(isCont){
    # sample mutated contributions
    mutated_contribution = old_contribution + rnorm(n, 0, 1)
    # change the contribution value of those who choose to socially learn (switch their inherited c to mutated c)
    new_contribution = mutated_contribution*does_mutate + old_contribution*(1-does_mutate)
    new_contribution[which(new_contribution<0)]=0
    new_contribution[which(new_contribution>=b)]=b
  } else {
      # sample mutated contributions
      mutated_contribution = old_contribution+sample(c(-1,1), n, replace = TRUE)
      # change the contribution value of those who choose to socially learn (switch their inherited c to mutated c)
      new_contribution = mutated_contribution*does_mutate + old_contribution*(1-does_mutate)
      new_contribution[which(new_contribution==-1)]=1
      new_contribution[which(new_contribution==10)]=8
  }
  return(new_contribution)
}

# Description:
# this function calculates the payoff of each individual in the population depending on their contribution
#
# Arguments:
# - contribution is a vector of length n (number of individuals in the population) containing the contribution of each individual in the population
# - m is the value of the multiplier
# - n is the number of individuals in the population
#
# Return:
# a vector of length n (number of individuals) that contains the payoff (amount everyone gets back) in the population
payoff = function(contribution, m, n){
  return((m*sum(contribution))/n)
}

# Description:
#this function calculates the penalty of each individual in the population depending on their contribution
#
# Arguments:
# - contribution is a vector of length n (number of individuals in the population) containing the contribution of each individual in the population
# - c_norm is the value of the contribution norm (either minimum threshold or average)
# - norm_intensity is the value of the intensity of the norm (t->l)
# - isAdditive is a boolean variable; if TRUE, penalty is additive, if FALSE, it is multiplicative
# - n is the number of individuals in the population
#
# Return:
# a vector of length n (number of individuals) that contains the penalty of each individual in the population
penalty = function(contribution, c_norm, norm_intensity, isAdditive, n){
  if(isAdditive){
    #degree of deviance of each individual in the population
    dev = abs(contribution-c_norm)
    #calculate penalty value
    p = dev*norm_intensity
    #those below the contribution norm get assigned penalties
    #those equal or above will receive no penalty(=0)
    penalty = ifelse(contribution<c_norm, p, 0)
  } else{
    #calculate penalty. Here, the penalty will be a value between 0 and 1. The higher the value, the lower the penalty
    p = dnorm(contribution, c_norm, 1/norm_intensity)/dnorm(c_norm, c_norm, 1/norm_intensity)
    #those below the contribution norm get assigned penalties
    #those equal or above will receive no penalty(=1)
    penalty = ifelse(contribution<c_norm, p, 1)
  }
  return(penalty)
}

# Description:
# this function calculates the fitness of each individual in the population depending on their contribution, payoff, and penalty
# (those who contributed more will have a lower fitness since everyone got back the same amount,
# so contributing more is costly)
# 
# Arguments:
# - contribution is a vector of length n (number of individuals in the population) containing the contribution of each individual in the population
# - b is the value of the baseline (score everyone starts with in the beginning)
# - m is the value of the multiplier
# - n is the number of individuals in the population
# - c_norm is the value of the contribution norm (either minimum threshold or average)
# - norm_intensity is the value of the intensity of the norm (t->l)
# - isAdditive is a boolean variable; if TRUE, penalty is additive, if FALSE, it is multiplicative
#
# Return:
# a vector of length n (number of individuals) that contains the fitness of each individual in the population
f = function(contribution, b, m, n, c_norm, norm_intensity, isAdditive){
  #calculate the score of each individual based on their contribution and payoff
  score = b - contribution + payoff(contribution, m, n)
  #adjust scores based on penalties:
  #if penalty is additive, subtract it from the score
  #if penalty is multiplicative, multiply it to the score
  if(isAdditive){
    score = score - penalty(contribution, c_norm, norm_intensity, isAdditive)
  } else {
    score = score*penalty(contribution, c_norm, norm_intensity, isAdditive)
  }
  #set negative values to zero
  score = pmax(score, 0)
  #convert values to probabilities (fitness)
  return(score/sum(score))
}

# Description:
# this function is intended for running the pgg_model() function multiple times for different norm intensity values
#
# Arguments:
# - norm_intensities is a vector of norm intensity values
# - FUN is the function to be applied to each element of norm_intensities. It is intended for the pgg_model() function
# - ... is to allow changes in the arguments of the FUN function
#
# Return:
# a list of vectors of size length(norm_intensities) containing average contributions according to the given norm intensity values
multiple_intensities = function(norm_intensities = 1:2, FUN, ...){
  conts = vector(mode = "list", length = length(norm_intensities))
  for(norm_intensity in norm_intensities){
    ind = match(norm_intensity, norm_intensities)
    conts[[ind]] = FUN(norm_intensity = norm_intensity, ...)
  }
  return(conts)
}

# Description:
# this function is intended for running the pgg_model() function multiple times for different c_min values
#
# Arguments:
# - c_mins is a vector of c_min values
# - FUN is the function to be applied to each element of c_mins. It is intended for the pgg_model() function
# - ... is to allow changes in the arguments of the FUN function
#
# Return:
# a list of vectors of size length(c_mins) containing average contributions according to the given c_min values
multiple_cmins = function(c_mins = c(3, 6, 8), FUN, ...){
  conts = vector(mode = "list", length = length(c_mins))
  for(c_min in c_mins){
    ind = match(c_min, c_mins)
    conts[[ind]] = FUN(c_min = c_min, ...)
  }
}
