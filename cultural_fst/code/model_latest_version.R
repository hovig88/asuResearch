# start = Sys.time()
#load library
library(rjags)
library(HDInterval)
library(RColorBrewer)

#read csv file
data = read.csv("../data/fst_data_coded.csv", header = T)
data = data[,c(1:11,11+order(apply(data[,12:60], MARGIN = 2, var), decreasing = TRUE))]
data$Y = data[,12:60]

ethnic_group_of_clan <- vector()
for (i in 0:8) {
  ethnic_group_of_clan <- c(ethnic_group_of_clan, unique(data$ethnic_group[data$clan == i]))
}
clan_of_subclan <- vector()
for (i in 0:33) {
  clan_of_subclan <- c(clan_of_subclan, unique(data$clan[data$subclan == i]))
}

data_list <- list(
  ethnic_group_of_clan=ethnic_group_of_clan,
  clan_of_subclan=clan_of_subclan,
  Y=data$Y,
  subclan=data$subclan,
  clan=data$clan,
  territorial_section=data$territorial_section,
  ethnic_group=data$ethnic_group,
  sex=data$sex,
  school_yrs=data$school_yrs,
  town_yrs=data$town_yrs,
  marital_status=data$marital_status
)

#building the model in JAGS
model_string = "
model {
  for (j in c(1:49)) {
  
    ## LIKELIHOOD ##
    
    for (i in 1:745) {
      Y[i,j] ~ dbern(p[i,j])
      logit(p[i,j]) <- subclan_effect[subclan[i] + 1, j]*(1-equals(ethnic_group[i], 3)) +
                       (clan_effect[clan[i] + 1,j] + territorial_section_effect[territorial_section[i] + 1, j])*equals(ethnic_group[i], 3) + 
                       beta[2,j]*sex[i] +
                       beta[3,j]*school_yrs[i] +
                       beta[4,j]*town_yrs[i] +
                       marital_status_effect[marital_status[i] + 1,j]
    }
    
    ## PRIORS ##
    
    for (i in 1:34) {
      subclan_effect[i,j] ~ dnorm(clan_effect[clan_of_subclan[i] + 1, j], 1/sd_subclan^2)
    }
    for (i in c(1:9)) {
      clan_effect[i,j] ~ dnorm(ethnic_group_effect[ethnic_group_of_clan[i]+1,j], 1/sd_clan^2)
    }
    for (i in 1:4) {
      territorial_section_effect[i,j] ~ dnorm(0, 1/sd_territorial_section^2)
    }
    for (i in 1:4) {
      ethnic_group_effect[i,j] ~ dnorm(beta[1,j], 1/sd_ethnic_group^2)
    }
    beta[1,j] ~ dnorm(0, 1/0.75^2)
    for(i in 2:4){
      beta[i,j] ~ dnorm(0, 1/0.5^2)
    }
    for (i in 1:5) {
      marital_status_effect[i,j] ~ dnorm(0, 1/sd_marital_status^2)
    }
  }
  
  ## HYPERPRIORS ##
  
  sd_ethnic_group ~ dexp(3)
  sd_territorial_section ~ dexp(3)
  sd_clan ~ dexp(3)
  sd_subclan ~ dexp(3)
  sd_marital_status ~ dexp(3)
} "

#running the model in JAGS
n.chains = 3
n.iter = 1000
model <- jags.model(textConnection(model_string), data=data_list, n.chains=n.chains)
nodes <- c("beta", "marital_status_effect", "sd_marital_status",
           "ethnic_group_effect", "sd_ethnic_group",
           "territorial_section_effect", "sd_territorial_section",
           "clan_effect", "sd_clan")
samples <- coda.samples(model=model, variable.names=nodes, n.iter=n.iter, thin=1)

# end = Sys.time()
# 
# end - start

#assessing model performance
gelman.diag(samples)
autocorr.diag(samples)
crosscorr.plot(samples)
effectiveSize(samples)
hist(effectiveSize(samples))
range(effectiveSize(samples))
summary(samples)

#explore crosscorrelations
source("./crosscorrelations.R")

#prior predictive check
prior_params = data.frame(baseline = c(0,0.75), sd_ethnic_group = 4, sd_clan = 4, sex = c(0,0.5), school_yrs = c(0,0.5),
                          town_yrs = c(0,0.5), marital_status = c(0,2), territorial_section = c(0,2))
pcc(prior_params = prior_params)

#exploring results
hist(rexp(10000,sd(data$clan)), xlim = c(0,10))
hist(rexp(10000,1), add = T, col = rgb(1,0,0,0.25))
hist(extract("sd_clan", samples), add = T, col = rgb(0,1,0,0.25))

plot(extract("base_clan[4,1]", samples))
hist(extract("base_clan[4,1]", samples))

plot(extract("base_clan[4,1]", samples), extract("territorial_section_effect[2,1]", samples))
abline(lm(extract("territorial_section_effect[2,1]", samples) ~ extract("base_clan[4,1]", samples)), lwd = 2, col = "red")
plot(extract("base_clan[4,1]", samples), extract("territorial_section_effect[3,1]", samples))
abline(lm(extract("territorial_section_effect[3,1]", samples) ~ extract("base_clan[4,1]", samples)), lwd = 2, col = "red")
plot(extract("base_clan[4,1]", samples), extract("territorial_section_effect[4,1]", samples))
abline(lm(extract("territorial_section_effect[4,1]", samples) ~ extract("base_clan[4,1]", samples)), lwd = 2, col = "red")

plot(extract("base_clan[5,1]", samples), extract("territorial_section_effect[2,1]", samples))
abline(lm(extract("territorial_section_effect[2,1]", samples) ~ extract("base_clan[5,1]", samples)), lwd = 2, col = "red")
plot(extract("base_clan[5,1]", samples), extract("territorial_section_effect[3,1]", samples))
abline(lm(extract("territorial_section_effect[3,1]", samples) ~ extract("base_clan[5,1]", samples)), lwd = 2, col = "red")
plot(extract("base_clan[5,1]", samples), extract("territorial_section_effect[4,1]", samples))
abline(lm(extract("territorial_section_effect[4,1]", samples) ~ extract("base_clan[5,1]", samples)), lwd = 2, col = "red")

plot(extract("base_clan[6,1]", samples), extract("territorial_section_effect[2,1]", samples))
plot(extract("base_clan[6,1]", samples), extract("territorial_section_effect[3,1]", samples))
plot(extract("base_clan[6,1]", samples), extract("territorial_section_effect[4,1]", samples))

plot(extract("base_clan[5,1]", samples))
plot(extract("base_clan[6,1]", samples))
plot(extract("territorial_section_effect[2,1]", samples))
plot(extract("territorial_section_effect[3,1]", samples))
plot(extract("territorial_section_effect[4,1]", samples))


#we expect to see fuzziness from the following, but we don't:
# plot(extract("sd_marital_status", samples))
# 
# hist(extract("sd_marital_status", samples))
# 
# #marital status doesn't capture variation well across all individuals for the norm at column 27
# plot(extract("sd_marital_status", samples), extract("marital_status_effect[1,27]", samples))
# plot(extract("sd_marital_status", samples), extract("marital_status_effect[2,27]", samples))
# plot(extract("sd_marital_status", samples), extract("marital_status_effect[3,27]", samples))
# plot(extract("sd_marital_status", samples), extract("marital_status_effect[4,27]", samples))
# plot(extract("sd_marital_status", samples), extract("marital_status_effect[5,27]", samples))
# #what about column 26? (KIDNAP - has most variation)
# plot(extract("sd_marital_status", samples), extract("marital_status_effect[1,26]", samples))
# plot(extract("sd_marital_status", samples), extract("marital_status_effect[2,26]", samples))
# plot(extract("sd_marital_status", samples), extract("marital_status_effect[3,26]", samples))
# plot(extract("sd_marital_status", samples), extract("marital_status_effect[4,26]", samples))
# plot(extract("sd_marital_status", samples), extract("marital_status_effect[5,26]", samples))
# 
# hist(rexp(10000, 2))
# hist(rexp(10000, 4))

responses = c(1,49)

#baseline
baseline = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
#ethnic group effect
borana_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
rendille_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
samburu_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
turkana_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
#territorial section effect
no_ts_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
kwatela_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
ngibochoros_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
ngiyapakuno_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
#clan effect
ldupsai_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
lpisikishu_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
lukumai_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
ngidoca_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
ngipongaa_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
ngisiger_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
noonituu_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
saale_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))
warrajidaa_effect = as.data.frame(matrix(nrow = n.chains*n.iter, ncol = length(responses)))



for(j in 1:length(responses)){
  baseline[,j] = extract(paste0("beta[1,",responses[j],"]"), samples)
  borana_effect[,j] = extract(paste0("ethnic_group_effect[1,",responses[j],"]"), samples)
  rendille_effect[,j] = extract(paste0("ethnic_group_effect[2,",responses[j],"]"), samples)
  samburu_effect[,j] = extract(paste0("ethnic_group_effect[3,",responses[j],"]"), samples)
  turkana_effect[,j] = extract(paste0("ethnic_group_effect[4,",responses[j],"]"), samples)
  no_ts_effect[,j] = extract(paste0("territorial_section_effect[1,",responses[j],"]"), samples)
  kwatela_effect[,j] = extract(paste0("territorial_section_effect[2,",responses[j],"]"), samples)
  ngibochoros_effect[,j] = extract(paste0("territorial_section_effect[3,",responses[j],"]"), samples)
  ngiyapakuno_effect[,j] = extract(paste0("territorial_section_effect[4,",responses[j],"]"), samples)
  ldupsai_effect[,j] = extract(paste0("base_clan[1,",responses[j],"]"), samples)
  lpisikishu_effect[,j] = extract(paste0("base_clan[2,",responses[j],"]"), samples)
  lukumai_effect[,j] = extract(paste0("base_clan[3,",responses[j],"]"), samples)
  ngidoca_effect[,j] = extract(paste0("base_clan[4,",responses[j],"]"), samples)
  ngipongaa_effect[,j] = extract(paste0("base_clan[5,",responses[j],"]"), samples)
  ngisiger_effect[,j] = extract(paste0("base_clan[6,",responses[j],"]"), samples)
  noonituu_effect[,j] = extract(paste0("base_clan[7,",responses[j],"]"), samples)
  saale_effect[,j] = extract(paste0("base_clan[8,",responses[j],"]"), samples)
  warrajidaa_effect[,j] = extract(paste0("base_clan[9,",responses[j],"]"), samples)
}

hist(logit2prob(baseline[,1]), xlim = c(0,1), ylim = c(0,3000))
hist(logit2prob(rnorm(10000,0,1)), add = T, col = rgb(1,0,0,0.25))

hist(logit2prob(baseline[,2]), xlim = c(0,1), ylim = c(0,3000))
hist(logit2prob(rnorm(10000,0,1)), add = T, col = rgb(1,0,0,0.25))

#exploring effects between Turkana clans
p_c4 = logit2prob(ngidoca_effect)
p_c5 = logit2prob(ngipongaa_effect)
p_c6 = logit2prob(ngisiger_effect)

hist(p_c4[,1], xlim = c(0,1))
hist(p_c5[,1], xlim = c(0,1), add = T, col = rgb(1,0,0,0.25))
hist(p_c6[,1], xlim = c(0,1), add = T, col = rgb(0,1,0,0.25))

hist(p_c4[,2], xlim = c(0,1))
hist(p_c5[,2], xlim = c(0,1), add = T, col = rgb(1,0,0,0.25))
hist(p_c6[,2], xlim = c(0,1), add = T, col = rgb(0,1,0,0.25))

par(mfrow=c(3,2))
plot(p_c4[,1], p_c5[,1], main = "norm response 1")
abline(lm(p_c5[,1] ~ p_c4[,1]), lwd = 2, col = "red")

plot(p_c4[,2], p_c5[,2], main = "norm response 49")
abline(lm(p_c5[,2] ~ p_c4[,2]), lwd = 2, col = "red")


plot(p_c4[,1], p_c6[,1])
abline(lm(p_c6[,1] ~ p_c4[,1]), lwd = 2, col = "red")

plot(p_c4[,2], p_c6[,2])
abline(lm(p_c6[,2] ~ p_c4[,2]), lwd = 2, col = "red")


plot(p_c5[,1], p_c6[,1])
abline(lm(p_c6[,1] ~ p_c5[,1]), lwd = 2, col = "red")

plot(p_c5[,2], p_c6[,2])
abline(lm(p_c6[,2] ~ p_c5[,2]), lwd = 2, col = "red")

par(mfrow=c(1,1))

#exploring effects between Turkana territorial sections
p_ts1 = logit2prob(kwatela_effect)
p_ts2 = logit2prob(ngibochoros_effect)
p_ts3 = logit2prob(ngiyapakuno_effect)

hist(p_ts1[,1], xlim = c(0,1))
hist(p_ts2[,1], xlim = c(0,1), add = T, col = rgb(1,0,0,0.25))
hist(p_ts3[,1], xlim = c(0,1), add = T, col = rgb(0,1,0,0.25))

hist(p_ts1[,2], xlim = c(0,1))
hist(p_ts2[,2], xlim = c(0,1), add = T, col = rgb(1,0,0,0.25))
hist(p_ts3[,2], xlim = c(0,1), add = T, col = rgb(0,1,0,0.25))

par(mfrow=c(3,2))
plot(p_ts1[,1], p_ts2[,1], main = "norm response 1")
abline(lm(p_ts2[,1] ~ p_ts1[,1]), lwd = 2, col = "red")

plot(p_ts1[,2], p_ts2[,2], main = "norm response 49")
abline(lm(p_ts2[,2] ~ p_ts1[,2]), lwd = 2, col = "red")


plot(p_ts1[,1], p_ts3[,1])
abline(lm(p_ts3[,1] ~ p_ts1[,1]), lwd = 2, col = "red")

plot(p_ts1[,2], p_ts3[,2])
abline(lm(p_ts3[,2] ~ p_ts1[,2]), lwd = 2, col = "red")


plot(p_ts2[,1], p_ts3[,1])
abline(lm(p_ts3[,1] ~ p_ts2[,1]), lwd = 2, col = "red")

plot(p_ts2[,2], p_ts3[,2])
abline(lm(p_ts3[,2] ~ p_ts2[,2]), lwd = 2, col = "red")

par(mfrow=c(1,1))

#exploring effects between Turkana clans and territorial sections

# p_c4_eg = logit2prob(ngidoca_effect+turkana_effect)
# p_c4_eg_b = logit2prob(ngidoca_effect+turkana_effect+baseline)
# p_c4_eg_b_ts0 = logit2prob(ngidoca_effect+turkana_effect+baseline + no_ts_effect)
# p_c4_eg_b_ts1 = logit2prob(ngidoca_effect+turkana_effect+baseline + kwatela_effect)
# p_c4_eg_b_ts2 = logit2prob(ngidoca_effect+turkana_effect+baseline + ngibochoros_effect)
# p_c4_eg_b_ts3 = logit2prob(ngidoca_effect+turkana_effect+baseline + ngiyapakuno_effect)

p_c4_ts1 = logit2prob(ngidoca_effect + kwatela_effect)
p_c4_ts2 = logit2prob(ngidoca_effect + ngibochoros_effect)
p_c4_ts3 = logit2prob(ngidoca_effect + ngiyapakuno_effect)

p_c5_ts1 = logit2prob(ngipongaa_effect + kwatela_effect)
p_c5_ts2 = logit2prob(ngipongaa_effect + ngibochoros_effect)
p_c5_ts3 = logit2prob(ngipongaa_effect+ ngiyapakuno_effect)

p_c6_ts1 = logit2prob(ngisiger_effect + kwatela_effect)
p_c6_ts2 = logit2prob(ngisiger_effect + ngibochoros_effect)
p_c6_ts3 = logit2prob(ngisiger_effect + ngiyapakuno_effect)

# hist(p_c4_eg[,1], add = T, col = rgb(1,0,0,0.25))
# hist(p_c4_eg_b[,1], add = T, col = rgb(1,0,0,0.25))
# hist(p_c4_eg_b_ts0[,1], add = T, col = rgb(1,0,0,0.25))
# hist(p_c4_eg_b_ts1[,1], add = T, col = rgb(1,0,0,0.25))
# hist(p_c4_eg_b_ts2[,1], add = T, col = rgb(1,0,0,0.25))
# hist(p_c4_eg_b_ts3[,1], add = T, col = rgb(1,0,0,0.25))

hist(p_c4[,1], xlim = c(0,1))
hist(p_c4_ts1[,1], add = T, col = rgb(1,0,0,0.25))
hist(p_c4_ts2[,1], add = T, col = rgb(1,0,0,0.25))
hist(p_c4_ts3[,1], add = T, col = rgb(1,0,0,0.25))

hist(p_c5[,1], xlim = c(0,1))
hist(p_c5_ts1[,1], add = T, col = rgb(1,0,0,0.25))
hist(p_c5_ts2[,1], add = T, col = rgb(1,0,0,0.25))
hist(p_c5_ts3[,1], add = T, col = rgb(1,0,0,0.25))

hist(p_c6[,1], xlim = c(0,1))
hist(p_c6_ts1[,1], add = T, col = rgb(1,0,0,0.25))
hist(p_c6_ts2[,1], add = T, col = rgb(1,0,0,0.25))
hist(p_c6_ts3[,1], add = T, col = rgb(1,0,0,0.25))

hist(p_c4[,2], xlim = c(0,1))
hist(p_c4_ts1[,2], add = T, col = rgb(1,0,0,0.25))
hist(p_c4_ts2[,2], add = T, col = rgb(1,0,0,0.25))
hist(p_c4_ts3[,2], add = T, col = rgb(1,0,0,0.25))

hist(p_c5[,2], xlim = c(0,1))
hist(p_c5_ts1[,2], add = T, col = rgb(1,0,0,0.25))
hist(p_c5_ts2[,2], add = T, col = rgb(1,0,0,0.25))
hist(p_c5_ts3[,2], add = T, col = rgb(1,0,0,0.25))

hist(p_c6[,2], xlim = c(0,1))
hist(p_c6_ts1[,2], add = T, col = rgb(1,0,0,0.25))
hist(p_c6_ts2[,2], add = T, col = rgb(1,0,0,0.25))
hist(p_c6_ts3[,2], add = T, col = rgb(1,0,0,0.25))

par(mfrow=c(3,3))
for(j in 1:length(responses)){
  plot(p_c4[,j], p_ts1[,j], xlim = c(0,1), ylim = c(0,1))
  abline(lm(p_ts1[,j] ~ p_c4[,j]), lwd = 2, col = "red")
  plot(p_c4[,j], p_ts2[,j], xlim = c(0,1), ylim = c(0,1))
  abline(lm(p_ts2[,j] ~ p_c4[,j]), lwd = 2, col = "red")
  plot(p_c4[,j], p_ts3[,j], xlim = c(0,1), ylim = c(0,1))
  abline(lm(p_ts3[,j] ~ p_c4[,j]), lwd = 2, col = "red")
  
  plot(p_c5[,j], p_ts1[,j], xlim = c(0,1), ylim = c(0,1))
  abline(lm(p_ts1[,j] ~ p_c5[,j]), lwd = 2, col = "red")
  plot(p_c5[,j], p_ts2[,j], xlim = c(0,1), ylim = c(0,1))
  abline(lm(p_ts2[,j] ~ p_c5[,j]), lwd = 2, col = "red")
  plot(p_c5[,j], p_ts3[,j], xlim = c(0,1), ylim = c(0,1))
  abline(lm(p_ts3[,j] ~ p_c5[,j]), lwd = 2, col = "red")
  
  plot(p_c6[,j], p_ts1[,j], xlim = c(0,1), ylim = c(0,1))
  abline(lm(p_ts1[,j] ~ p_c6[,j]), lwd = 2, col = "red")
  plot(p_c6[,j], p_ts2[,j], xlim = c(0,1), ylim = c(0,1))
  abline(lm(p_ts2[,j] ~ p_c6[,j]), lwd = 2, col = "red")
  plot(p_c6[,j], p_ts3[,j], xlim = c(0,1), ylim = c(0,1))
  abline(lm(p_ts3[,j] ~ p_c6[,j]), lwd = 2, col = "red")
}
par(mfrow=c(1,1))






