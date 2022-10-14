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
plot(data$agemate_no_report~data$ethnic_group, bty="n", pch=16, col="aquamarine3")
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
                   beta[1]*sex[i] +
                   marital_status_effect[marital_status[i] + 1] +
                   territorial_section_effect[territorial_section[i] + 1]*equals(ethnic_group[i], 3) +
                   beta[2]*attend_school[i] +
                   beta[3]*school_yrs[i] +
                   beta[4]*town[i] +
                   beta[5]*town_yrs[i]
  }
  
  for (i in 1:4) {
    baseline[i] ~ dnorm(0, 1/5^2)
  }
  for (i in 1:5) {
    marital_status_effect[i] ~ dnorm(0, 1/sd_marital_status^2)
  }
  sd_marital_status ~ dexp(2)
  for (i in 1:4) {
    territorial_section_effect[i] ~ dnorm(0, 1/sd_territorial_section^2)
  }
  sd_territorial_section ~ dexp(1)
  for(i in 1:5){
    beta[i] ~ dnorm(0, 1/0.5^2)
  }
} "

#running the model in JAGS
model <- jags.model(textConnection(model_string), data=data_subset, n.chains=3)
nodes <- c("baseline", "beta", "marital_status_effect", "sd_marital_status", "territorial_section_effect", "sd_territorial_section")
samples <- coda.samples(model,nodes,10000,2)

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
#baseline for each ethnic group
b0_Borana <- extract("baseline[1]", samples)
b0_Rendille <- extract("baseline[2]", samples)
b0_Samburu <- extract("baseline[3]", samples)
b0_Turkana <- extract("baseline[4]", samples)

sex_effect <- extract("beta[1]", samples)
attend_school_effect <- extract("beta[2]", samples)
school_yrs_effect <- extract("beta[3]", samples)
town_effect <- extract("beta[4]", samples)
town_yrs_effect <- extract("beta[5]", samples)

#marital status effect
ms_divorced_effect <- extract("marital_status_effect[1]", samples)
ms_married_effect <- extract("marital_status_effect[2]", samples)
ms_single_effect <- extract("marital_status_effect[3]", samples)
ms_unofficial_effect <- extract("marital_status_effect[4]", samples)
ms_widowed_effect <- extract("marital_status_effect[5]", samples)
ms_sd <- extract("sd_marital_status", samples)

#territorial section effect
no_ts_effect <- extract("territorial_section_effect[1]", samples)
ts_kwatela_effect <- extract("territorial_section_effect[2]", samples)
ts_ngibochoros_effect <- extract("territorial_section_effect[3]", samples)
ts_ngiyapakuno_effect <- extract("territorial_section_effect[4]", samples)

#create color palette for histogram plots
col_palette = brewer.pal(9, "Set1")

#histogram of baselines
x1 = b0_Borana
p1 = exp(x1)/(1+exp(x1))
hist(p1, col = NULL, xlim = c(0,1), border = col_palette[2])
#histogram of baselines
x1 = b0_Rendille
p1 = exp(x1)/(1+exp(x1))
hist(p1, col = NULL, border = col_palette[3], add = T)
#histogram of baselines
x1 = b0_Samburu
p1 = exp(x1)/(1+exp(x1))
hist(p1, col = NULL, border = col_palette[4], add = T)
#histogram of baselines
x1 = b0_Turkana
p1 = exp(x1)/(1+exp(x1))
hist(p1, col = NULL, border = col_palette[5], add = T)

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
