#load library
library(rjags)
library(HDInterval)

# set working directory
#setwd("~/Desktop/Cultural FST dataset for Hovig Project/data/")

# Yes[i] ~ dbern(p[i])
# logit(p[i]) = group - baseline + geo_effect + schooling + town + sex + age + marital status
data = read.csv("FST_dataset_culled_FINAL_with_demography_for_Hovig.csv", header = T, quote = "",
                stringsAsFactors = TRUE)

#ethnic group (0 -> 3)
ethnic_group = as.numeric(data$ETHNIC_GROUP)-1
#sex (0 or 1)
sex = as.numeric(data$SEX)-1
# marital status (0 -> 4)
marital_status = as.numeric(data$MARITAL_STATUS)-1
#territorial section (Turkana only)
territorial_section = data$TERRITORIAL_SECTION
levels(territorial_section)[3]="999"
territorial_section=as.numeric(territorial_section)-1
#whether or not the individual has attended school
attend_school = data$ATTEND_SCHOOL
#number of school years
school_yrs = data$SCHOOL_YRS
#whether or not the individual has lived in a town
town = data$TOWN
#number of years lived in a town
town_yrs = data$TOWN_YRS

data_subset = data.frame(raiding_blessings, ethnic_group, sex, marital_status)

#hierarchical model
#school_location = data$SCHOOL_LOCATION
#town_location = data$TOWN_LOCATION

#norm
raiding_blessings = as.numeric(data$RAIDING_BLESSINGS)-2


data = subset(data, raiding_blessings != -1)
data = subset(data, raiding_blessings != 2)

#data
data_subset = list(N=length(data$ethnic_group), ethnic_group=data$ethnic_group, 
                   sex=data$sex, marital_status=data$marital_status)

#building the model in JAGS
model_string = "model {
  for (i in 1:N) {
    raiding_blessings[i] ~ dbern(p[i])
    logit(p[i]) <- beta[1] +
                   beta[2]*(ethnic_group[i]-mean(ethnic_group)) +
                   beta[3]*(sex[i]-mean(sex)) +
                   beta[4]*(marital_status[i]-mean(marital_status))
  }
  
  beta[1] ~ dnorm(0,1/3^2)
  for(i in 2:4){
    beta[i] ~ dnorm(0, 1/5^2)
  }
} "

#running the model in JAGS
model <- jags.model(textConnection(model_string), data=data_subset, n.chains=3)
nodes <- c("beta")
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

hist(base, ylim = c(0, 10000), xlim = c(-30,30))
hist(base+ethnic_group_effect, col = NULL, border = "red", add = T)
hist(base+sex_effect, col = NULL, border = "blue", add = T)
hist(base+marital_status_effect, col = NULL, border = "green", add = T)
hist(base+ethnic_group_effect+sex_effect+marital_status_effect, col = NULL, border = "yellow", add = T)

