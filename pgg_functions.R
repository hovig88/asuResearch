#some functions used in the public goods game model

#this function samples contributions for individuals in the new generation based on the contributions of the previous generation and their relative fitness
# arguments:
# - parent_contribution is a vector of length n (number of individuals in the population) containing the contribution of each individual in the previous generation
# - n is the number of individuals in the population
# - parent_fitness is a vector of length n (number of individuals in the population) containing the fitness of each individual in the previous generation
#this function returns a vector of length n (number of individuals) that contains the contributions of each individual in the new generation
inherited_c = function(parent_contribution, n, parent_fitness = NULL){
  return(sample(parent_contribution, n, replace = TRUE, parent_fitness))
}

#this function calculates the contribution of each individual in the population after mutation is applied
# arguments:
# - old_contribution is a vector of length n (number of individuals in the population) containing the contribution of each individual in the population before mutation is applied
# - c is a vector containing the possible contribution values (trait options) in the population
# - n is the number of individuals in the population
# - mu is the value of the mutation rate
#this function returns a vector of length n (number of individuals) that contains the contributions of each individual in the population after mutation is applied
mutation = function(old_contribution, c, n, mu){
  # sample mutated contributions
  mutated_contribution = sample(c, n, replace = TRUE)
  # determine which of the individuals will have the mutated contribution (a vector of 0s and 1s)
  does_mutate = (runif(n, 0, 1)<mu)*1
  # change the contribution value of those who choose to socially learn (switch their inherited c to mutated c)
  new_contribution = mutated_contribution*does_mutate + old_contribution*(1-does_mutate)
  return(new_contribution)
}

#this function calculates the contribution of each individual in the population after symmetric mutation is applied
# arguments:
# - old_contribution is a vector of length n (number of individuals in the population) containing the contribution of each individual in the population before mutation is applied
# -isCont is a boolean variable that will be TRUE if the contributions are continuous, FALSE if discrete
# - n is the number of individuals in the population
# - mu is the value of the mutation rate
#this function returns a vector of length n (number of individuals) that contains the contributions of each individual in the population after symmetric mutation is applied
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

#this function calculates the payoff of each individual in the population depending on their contribution
# arguments:
# - contribution is a vector of length n (number of individuals in the population) containing the contribution of each individual in the population
# - m is the value of the multiplier
# - n is the number of individuals in the population
#this function returns a vector of length n (number of individuals) that contains the payoff (amount everyone gets back) in the population
payoff = function(contribution, m, n){
  return((m*sum(contribution))/n)
}

#this function calculates the penalty of each individual in the population depending on their contribution
# arguments:
# - contribution is a vector of length n (number of individuals in the population) containing the contribution of each individual in the population
# - c_min is the value of the minimum expected contribution (the norm)
# - norm_intensity is the value of the intensity of the norm (t->l)
#this function returns a vector of length n (number of individuals) that contains the penalty of each individual in the population
penalty = function(contribution, c_min, norm_intensity){
  #degree of deviance of each individual in the population
  dev = abs(contribution-c_min)
  return(ifelse(contribution<c_min, dev*norm_intensity, 0))
}

#this function calculates the fitness of each individual in the population depending on their contribution, payoff, and penalty
# (those who contributed more will have a lower fitness since everyone got back the same amount,
# so contributing more is costly)
# arguments:
# - contribution is a vector of length n (number of individuals in the population) containing the contribution of each individual in the population
# - b is the value of the baseline (score everyone starts with in the beginning)
# - m is the value of the multiplier
# - n is the number of individuals in the population
# - c_min is the value of the minimum expected contribution (the norm)
# - norm_intensity is the value of the intensity of the norm (t->l)
#this function returns a vector of length n (number of individuals) that contains the fitness of each individual in the population
f = function(contribution, b, m, n, c_min, norm_intensity){
  #calculate the score of each individual based on their contribution, payoff, and penalty
  score = b - contribution + payoff(contribution, m, n) - penalty(contribution, c_min, norm_intensity)
  #set negative values to zero
  score = pmax(score, 0)
  #convert values to probabilities (fitness)
  return(score/sum(score))
}
