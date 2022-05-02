## function to extract samples generated from the markov chains
extract <- function(var, samples) {
  n_chains <- length(samples)
  samps <- vector()
  for (i in 1:n_chains) {
    samps <- c(samps, samples[[i]][,var])
  }
  return(samps)
}

## function to convert logits to probabilty
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
