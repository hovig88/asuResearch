# start = Sys.time()

#load library
library(rjags)
library(HDInterval)
library(RColorBrewer)

#read csv file
data = read.csv("../data/fst_data_coded.csv", header = T)
data = data[,c(1:11,11+order(apply(data[,12:60], MARGIN = 2, var), decreasing = TRUE))]
data$Y = data[,12:60]

#building the model in JAGS
model_string = "
model {
  for (j in c(1,49)){
    for (i in 1:745) {
      Y[i,j] ~ dbern(p[i,j])
      logit(p[i,j]) <- beta[1,j] +
                     beta[2,j]*sex[i] +
                     beta[3,j]*school_yrs[i] +
                     beta[4,j]*town_yrs[i] +
                     marital_status_effect[marital_status[i] + 1,j] +
                     ethnic_group_effect[ethnic_group[i] + 1,j] +
                     territorial_section_effect[territorial_section[i] + 1,j]*equals(ethnic_group[i], 3) +
                     clan_effect[clan[i] + 1,j] +
                     subclan_effect[subclan[i] + 1,j]*(1 - equals(ethnic_group[i], 3))
    }
    
    beta[1,j] ~ dnorm(0, 1/1^2)
    for(i in 2:4){
      beta[i,j] ~ dnorm(0, 1/0.5^2)
    }
    for (i in 1:5) {
      marital_status_effect[i,j] ~ dnorm(0, 1/sd_marital_status^2)
    }
    for (i in 1:4) {
      ethnic_group_effect[i,j] ~ dnorm(0, 1/sd_ethnic_group^2)
    }
    for (i in 1:4) {
      territorial_section_effect[i,j] ~ dnorm(0, 1/sd_territorial_section^2)
    }
    for (i in 1:9) {
      clan_effect[i,j] ~ dnorm(0, 1/sd_clan^2)
    }
    for (i in 1:34) {
      subclan_effect[i,j] ~ dnorm(0, 1/sd_subclan^2)
    }
  }
  sd_ethnic_group ~ dexp(2)
  sd_territorial_section ~ dexp(2)
  sd_clan ~ dexp(2)
  sd_subclan ~ dexp(2)
  sd_marital_status ~ dexp(2)
} "

#running the model in JAGS
n.chains = 3
n.iter = 1000
model <- jags.model(textConnection(model_string), data=data, n.chains=n.chains)
nodes <- c("beta", "marital_status_effect", "sd_marital_status",
           "ethnic_group_effect", "sd_ethnic_group",
           "territorial_section_effect", "sd_territorial_section",
           "clan_effect", "sd_clan",
           "subclan_effect", "sd_subclan")
samples <- coda.samples(model=model, variable.names=nodes, n.iter=n.iter, thin=1)

# end = Sys.time()

# end - start

gelman.diag(samples)
autocorr.diag(samples)
crosscorr.plot(samples)
effectiveSize(samples)
hist(effectiveSize(samples))
range(effectiveSize(samples))

summary(samples)

#we expect to see fuzziness from the following, but we don't:
plot(extract("sd_marital_status", samples))

hist(extract("sd_marital_status", samples))

#marital status doesn't capture variation well across all individuals for the norm at column 27
plot(extract("sd_marital_status", samples), extract("marital_status_effect[1,27]", samples))
plot(extract("sd_marital_status", samples), extract("marital_status_effect[2,27]", samples))
plot(extract("sd_marital_status", samples), extract("marital_status_effect[3,27]", samples))
plot(extract("sd_marital_status", samples), extract("marital_status_effect[4,27]", samples))
plot(extract("sd_marital_status", samples), extract("marital_status_effect[5,27]", samples))
#what about column 26? (KIDNAP - has most variation)
plot(extract("sd_marital_status", samples), extract("marital_status_effect[1,26]", samples))
plot(extract("sd_marital_status", samples), extract("marital_status_effect[2,26]", samples))
plot(extract("sd_marital_status", samples), extract("marital_status_effect[3,26]", samples))
plot(extract("sd_marital_status", samples), extract("marital_status_effect[4,26]", samples))
plot(extract("sd_marital_status", samples), extract("marital_status_effect[5,26]", samples))

hist(rexp(10000, 2))
hist(rexp(10000, 4))


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
ts_sd <- extract("sd_territorial_section", samples)
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
