#extract samples generated from the markov chains
extract <- function(var, samples) {
  n_chains <- length(samples)
  samps <- vector()
  for (i in 1:n_chains) {
    samps <- c(samps, samples[[i]][,var])
  }
  return(samps)
}

#convert logits to probabilty
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#prior predictive check (according to old model structure)
ppc <- function(prior_params){
  prior_pc = data.frame(baseline_prior = rnorm(10000, prior_params[1,1], rexp(10000,prior_params[2,1])),
                        sex_prior = rnorm(10000, prior_params[1,2], prior_params[2,2]),
                        attend_school_prior = rnorm(10000, prior_params[1,3], prior_params[2,3]),
                        school_yrs_prior = rnorm(10000, prior_params[1,4], prior_params[2,4]),
                        town_prior = rnorm(10000, prior_params[1,5], prior_params[2,5]),
                        town_yrs_prior = rnorm(10000, prior_params[1,6], prior_params[2,6]),
                        marital_status_prior = rnorm(10000, prior_params[1,7], rexp(10000,prior_params[2,7])),
                        ethnic_group_prior = rnorm(10000, prior_params[1,8], rexp(10000,prior_params[2,8])),
                        territorial_section_prior = rnorm(10000, prior_params[1,9], rexp(10000,prior_params[2,9])),
                        clan_prior = rnorm(10000, prior_params[1,10], rexp(10000,prior_params[2,10])),
                        subclan_prior = rnorm(10000, prior_params[1,11], rexp(10000,prior_params[2,11])))
  p_prior = logit2prob(prior_pc)
  
  p_prior_sumStats = data.frame(p_prior_mean = apply(p_prior, MARGIN = 2, mean),
                                p_prior_median = apply(p_prior, MARGIN = 2, median),
                                p_prior_sd = apply(p_prior, MARGIN = 2, sd),
                                p_prior_hdi_lower = apply(p_prior, MARGIN = 2, hdi)[1,],
                                p_prior_hdi_higher = apply(p_prior, MARGIN = 2, hdi)[2,],
                                p_prior_range_lower = apply(p_prior, MARGIN = 2, range)[1,],
                                p_prior_range_higher = apply(p_prior, MARGIN = 2, range)[2,])
  names.params = c("Baseline", "Sex", "Attend school", "School years", "Town", "Town years", "Marital status",
                   "Ethnic group", "Territorial section", "Clan", "Subclan")
  
  par(mfrow=c(4,3))
  plot(p_prior_sumStats$p_prior_mean, type = "l", lwd = 2, col = "red", xaxt = "n", ylim = c(0,1), bty = "n",
       main = "Prior predictive check of model parameters", 
       xlab = "", ylab = "p(DISAGREE)")
  
  lines(p_prior_sumStats$p_prior_median, type = "l", lwd = 2, col = "blue")
  
  polygon(c(1:11,11:1), c(p_prior_sumStats$p_prior_hdi_lower,rev(p_prior_sumStats$p_prior_hdi_higher)),
          col = rgb(1,0,0,0.1), border = NA)
  polygon(c(1:11,11:1), c(p_prior_sumStats$p_prior_range_lower,rev(p_prior_sumStats$p_prior_range_higher)),
          col = rgb(1,0,0,0.1), border = NA)
  
  axis(side = 1, at = c(1:11), labels = names.params)
  
  for(i in 1:11){
    hist(p_prior[,i], xlim = c(0,1), main = names.params[i], xlab = "")
  }
}

#prior predictive check (according to new model structure)
ppc_new <- function(prior_params){
  prior_pc = data.frame(clan_prior = rnorm(10000, rnorm(10000,rnorm(10000,prior_params[1,1],
                                                                    prior_params[2,1]),
                                                        rexp(10000,prior_params[2,2])), 
                                           rexp(10000,prior_params[2,3])),
                        sex_prior = rnorm(10000, prior_params[1,4], prior_params[2,4]),
                        school_yrs_prior = rnorm(10000, prior_params[1,5], prior_params[2,5]),
                        town_yrs_prior = rnorm(10000, prior_params[1,6], prior_params[2,6]),
                        marital_status_prior = rnorm(10000, prior_params[1,7], rexp(10000,prior_params[2,7])),
                        territorial_section_prior = rnorm(10000, prior_params[1,8], rexp(10000,prior_params[2,8])))
  p_prior = logit2prob(prior_pc)
  
  p_prior_sumStats = data.frame(p_prior_mean = apply(p_prior, MARGIN = 2, mean),
                                p_prior_median = apply(p_prior, MARGIN = 2, median),
                                p_prior_sd = apply(p_prior, MARGIN = 2, sd),
                                p_prior_hdi_lower = apply(p_prior, MARGIN = 2, hdi)[1,],
                                p_prior_hdi_higher = apply(p_prior, MARGIN = 2, hdi)[2,],
                                p_prior_range_lower = apply(p_prior, MARGIN = 2, range)[1,],
                                p_prior_range_higher = apply(p_prior, MARGIN = 2, range)[2,])
  names.params = c("Clan", "Sex", "School years", "Town years", "Marital status", "Territorial section")
  
  layout(matrix(c(1,1,2:7), 4, 2, byrow = TRUE))
  plot(p_prior_sumStats$p_prior_mean, type = "l", lwd = 2, col = "red", xaxt = "n", ylim = c(0,1), bty = "n",
       main = "Prior predictive check of model parameters", 
       xlab = "", ylab = "p(DISAGREE)")
  
  lines(p_prior_sumStats$p_prior_median, type = "l", lwd = 2, col = "blue")
  
  polygon(c(1:dim(p_prior_sumStats)[1],dim(p_prior_sumStats)[1]:1), 
          c(p_prior_sumStats$p_prior_hdi_lower,rev(p_prior_sumStats$p_prior_hdi_higher)),
          col = rgb(1,0,0,0.1), border = NA)
  polygon(c(1:dim(p_prior_sumStats)[1],dim(p_prior_sumStats)[1]:1), 
          c(p_prior_sumStats$p_prior_range_lower,rev(p_prior_sumStats$p_prior_range_higher)),
          col = rgb(1,0,0,0.1), border = NA)
  
  axis(side = 1, at = c(1:dim(p_prior_sumStats)[1]), labels = names.params)
  
  for(i in 1:dim(p_prior_sumStats)[1]){
    hist(p_prior[,i], xlim = c(0,1), main = names.params[i], xlab = "", ylab= "", sub = )
  }
}

