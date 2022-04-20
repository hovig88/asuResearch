#load library
library(rjags)
library(HDInterval)
library(RColorBrewer)

#read csv file
data = read.csv("../data/cultural_fst_age_no_report.csv", header = T)

#data
data_subset = list(N=length(data$ethnic_group), agemate_no_report = data$agemate_no_report, 
                   ethnic_group=data$ethnic_group, sex=data$sex, marital_status=data$marital_status, 
                   territorial_section=data$territorial_section, attend_school=data$attend_school, 
                   school_yrs=data$school_yrs, town=data$town, town_yrs=data$town_yrs)

# check priors
plot(agemate_no_report~ethnic_group, bty="n", pch=16, col="aquamarine3")
ints <- rnorm(500, 0, 5)
slopes <- rnorm(500, 0, 0.5)
x <- seq(0, 10, 0.1)
for (i in 1:500) {
  lp <- ints[i] + slopes[i]*x
  y <- exp(lp)/(1+exp(lp))
  lines(y~x, col=rgb(0, 0, 0, 0.1))
}

#building the model in JAGS
model_string = "model {
  for (i in 1:N) {
    agemate_no_report[i] ~ dbern(p[i])
    logit(p[i]) <- baseline[ethnic_group[i] + 1] +
                   beta[3]*sex[i] +
                   beta[4]*marital_status[i] +
                   beta[5]*territorial_section[i] +
                   beta[6]*attend_school[i] +
                   beta[7]*school_yrs[i] +
                   beta[8]*town[i] +
                   beta[9]*town_yrs[i]
  }
  
  for (i in 1:4) {
    baseline[i] ~ dnorm(0, 1/5^2)
  }
  beta[1] ~ dnorm(0,1/5^2)
  for(i in 2:9){
    beta[i] ~ dnorm(0, 1/0.5^2)
  }
} "

#running the model in JAGS
model <- jags.model(textConnection(model_string), data=data_subset, n.chains=3)
nodes <- c("baseline", "beta")
samples <- coda.samples(model,nodes,5000,1)

gelman.diag(samples)
autocorr.diag(samples)
crosscorr.plot(samples)
effectiveSize(samples)

summary(samples)

#function to extract samples generated from the markov chains
extract <- function(var, samples) {
  n_chains <- length(samples)
  samps <- vector()
  for (i in 1:n_chains) {
    samps <- c(samps, samples[[i]][,var])
  }
  return(samps)
}

#extract the generated samples of each parameter
base <- extract("beta[1]", samples)
ethnic_group_effect <- extract("beta[2]", samples)
sex_effect <- extract("beta[3]", samples)
marital_status_effect <- extract("beta[4]", samples)
territorial_section_effect = extract("beta[5]", samples)
attend_school_effect = extract("beta[6]", samples)
school_yrs_effect = extract("beta[7]", samples)
town_effect = extract("beta[8]", samples)
town_yrs_effect = extract("beta[9]", samples)

#create color palette for histogram plots
col_palette = brewer.pal(10, "Paired")

#histogram of baseline
x1 = base
p1 = exp(x1)/(1+exp(x1))
hist(p1, col = NULL, xlim = c(0,0.6), border = col_palette[2])

#effect of ethnic group on the norm outcome
x2 = base + ethnic_group_effect
p2 = exp(x2)/(1+exp(x2))
hist(p2, col = NULL, border = col_palette[3], add = T)

#effect of sex on the norm outcome
x3 = base + sex_effect
p3 = exp(x3)/(1+exp(x3))
hist(p3, col = NULL, border = col_palette[4], add = T)

#effect of marital status on the norm outcome
x4 = base + marital_status_effect
p4 = exp(x4)/(1+exp(x4))
hist(p4, col = NULL, border = col_palette[5], add = T)

#effect of territorial section on the norm outcome
x5 = base + territorial_section_effect
p5 = exp(x5)/(1+exp(x5))
hist(p5, col = NULL, border = col_palette[6], add = T)

#effect of attending school on the norm outcome
x6 = base + attend_school_effect
p6 = exp(x6)/(1+exp(x6))
hist(p6, col = NULL, border = col_palette[7], add = T)

#effect of number of years in school on the norm outcome
x7 = base + school_yrs_effect
p7 = exp(x7)/(1+exp(x7))
hist(p7, col = NULL, border = col_palette[8], add = T)

#effect of town integration on the norm outcome
x8 = base + town_effect
p8 = exp(x8)/(1+exp(x8))
hist(p8, col = NULL, border = col_palette[9], add = T)

#effect of number of years lived in a town on the norm outcome
x9 = base + town_yrs_effect
p9 = exp(x9)/(1+exp(x9))
hist(p9, col = NULL, border = col_palette[10], add = T)

