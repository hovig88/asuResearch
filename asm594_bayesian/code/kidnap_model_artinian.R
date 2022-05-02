## --------------------------------
##
## Script name: kidnap_model_artinian
##
## Purpose of script: includes all of the code used for data analysis and producing the figures in my ASM 594 final report
##
## Author: Hovig Artinian
##
## Date Last Updated: 2022-05-01
##
## --------------------------------

## clear out all graphics devices and objects in the workspace
graphics.off()
rm(list=ls())

## --------------------------------

## set working directory (uncomment and change as needed)
## setwd("../code/")

## --------------------------------

## install and load packages (uncomment as needed)

## install.packages("rjags")
## install.packages("HDInterval")
## install.packages("RColorBrewer")

require(rjags)
require(HDInterval)
require(RColorBrewer)

## --------------------------------

## load up the functions to be used in this analysis
source("./functions_bayesian_artinian.R")

## --------------------------------

## to reproduce the results I obtained in my analysis, make sure to include the same seed value
set.seed(594)

## --------------------------------
## PALETTES TO BE USED IN PLOTS
## --------------------------------

palette_Paired = brewer.pal(12, "Paired")
palette_Dark2 = brewer.pal(8, "Dark2")
palette_Set1 = brewer.pal(9, "Set1")

## --------------------------------
## IMPORTING AND STORING DATASET
## --------------------------------

## change directory path as needed
data = read.csv("../data/data_kidnap.csv", header = T)

## store the columns in the dataset in a list
data_list = list(N=length(data$subject_id), kidnap = data$kidnap, 
                   ethnic_group=data$ethnic_group, sex=data$sex, marital_status=data$marital_status, 
                   territorial_section=data$territorial_section, 
                   school_yrs=data$school_yrs, town_yrs=data$town_yrs)

## --------------------------------
## VISUALIZING RAW DATA
## --------------------------------

## the following plot corresponds to Figure 1 in the report

graphics.off()
## start a pdf graphics device (uncomment and change as needed)
pdf(file = "../figures/figure1.pdf", width = 10)

par(mfrow= c(1, 2), oma = c(4,4,4,1), mar = c(0.5, 2, 1, 2), xpd = NA)
## individual responses per ethnic group
barplot(prop.table(table(data$kidnap, data$ethnic_group), margin = 2), beside = T,
        names.arg = c("Borana", "Rendille", "Samburu", "Turkana"), col = palette_Paired[c(2,6)], ylim = c(0,1))

text(-2, 1.125, "a", cex = 2)

## individual responses per territorial section
barplot(prop.table(table(data$kidnap, data$territorial_section)[,2:4], margin = 2), beside = T,
        names.arg = c("Kwatela", "Ngibochoros", "Ngiyapakuno"), col = palette_Paired[c(2,6)], ylim = c(0,1))

text(-1, 1.125, "b", cex = 2)

## plot legends
legend("topright", legend = c("Agree", "Disagree"), fill = palette_Paired[c(2,6)])

title(main = "", xlab = "", ylab = "Probability", outer = TRUE, line = 2.25)


## close current graphics device
dev.off()

## --------------------------------
## BUILDING THE MODEL IN JAGS
## --------------------------------

model_string = "model {
  
  ## likelihood:
  
  for (i in 1:N) {
    kidnap[i] ~ dbern(p[i])
    logit(p[i]) <- baseline[ethnic_group[i] + 1] +
                   marital_status_effect[marital_status[i] + 1] +
                   territorial_section_effect[territorial_section[i] + 1]*equals(ethnic_group[i], 3) +
                   beta[1]*sex[i] +
                   beta[2]*(school_yrs[i]) +
                   beta[3]*(town_yrs[i])
  }
  
  ## priors:
  
  for (i in 1:4) {
    baseline[i] ~ dnorm(0, 1/5^2)
  }
  for(i in 1:3){
    beta[i] ~ dnorm(0, 1/0.5^2)
  }
  for (i in 1:5) {
    marital_status_effect[i] ~ dnorm(0, 1/sd_marital_status^2)
  }
  for (i in 1:4) {
    territorial_section_effect[i] ~ dnorm(0, 1/sd_territorial_section^2)
  }
  
  ## hyperpriors:
  
  sd_marital_status ~ dexp(2)
  sd_territorial_section ~ dexp(1)
  
} "

## --------------------------------
## RUNNING THE MODEL IN JAGS
## --------------------------------

## initialize the model
model <- jags.model(textConnection(model_string), data=data_list, n.chains=3)
## save the names of the generated samples you want to analyze
nodes <- c("baseline", "beta", "marital_status_effect", "sd_marital_status", "territorial_section_effect", "sd_territorial_section")
## run the chains (take around 1 minute)
samples <- coda.samples(model,nodes,10000,2)


## --------------------------------
## MCMC DIAGNOSTICS
## --------------------------------

## check for convergence across the 3 MCMC chains
gelman.diag(samples)
## check for autocorrelation among estimates parameter values across all 3 chains 
autocorr.diag(samples)
## visualize the cross correlations between the estimated parameter values
crosscorr.plot(samples)
## check the effective sample size of each estimated parameter value
effectiveSize(samples)
## check summary statistics of estimated parameter values
summary(samples)

## --------------------------------
## EXTRACTING GENERATED SAMPLES
## --------------------------------

## baseline for each ethnic group
b0_Borana <- extract("baseline[1]", samples)
b0_Rendille <- extract("baseline[2]", samples)
b0_Samburu <- extract("baseline[3]", samples)
b0_Turkana <- extract("baseline[4]", samples)
## effect of being male
sex_effect <- extract("beta[1]", samples)
## number of years of schooling effect (per year effect)
school_yrs_effect <- extract("beta[2]", samples)
## number of years lived in a town effect (per year effect)
town_yrs_effect <- extract("beta[3]", samples)
## marital status effect
ms_divorced_effect <- extract("marital_status_effect[1]", samples)
ms_married_effect <- extract("marital_status_effect[2]", samples)
ms_single_effect <- extract("marital_status_effect[3]", samples)
ms_unofficial_effect <- extract("marital_status_effect[4]", samples)
ms_widowed_effect <- extract("marital_status_effect[5]", samples)
sd_ms <- extract("sd_marital_status", samples)
## territorial section effect
no_ts_effect <- extract("territorial_section_effect[1]", samples)
ts_kwatela_effect <- extract("territorial_section_effect[2]", samples)
ts_ngibochoros_effect <- extract("territorial_section_effect[3]", samples)
ts_ngiyapakuno_effect <- extract("territorial_section_effect[4]", samples)
sd_ts <- extract("sd_territorial_section", samples)

## --------------------------------
## PRIOR PREDICTIVE CHECK
## --------------------------------

## the following plot corresponds to Figure 2 in the report

## generate slope values (probabilities on the logit scale)
slope_B <- rnorm(500, 0, 0.5) + rnorm(500, 0, 0.5) + rnorm(500, 0, 0.5) + rnorm(500, 0, rexp(1,2))
slope_R <- rnorm(500, 0, 0.5) + rnorm(500, 0, 0.5) + rnorm(500, 0, 0.5) + rnorm(500, 0, rexp(1,2))
slope_S <- rnorm(500, 0, 0.5) + rnorm(500, 0, 0.5) + rnorm(500, 0, 0.5) + rnorm(500, 0, rexp(1,2))
slope_T <- rnorm(500, 0, 0.5) + rnorm(500, 0, 0.5) + rnorm(500, 0, 0.5) + rnorm(500, 0, rexp(1,2))

slope_Tk <- rnorm(500, 0, 0.5) + rnorm(500, 0, 0.5) + rnorm(500, 0, 0.5) + rnorm(500, 0, rexp(1,2)) + rnorm(500, 0, rexp(1,1))
slope_Tb <- rnorm(500, 0, 0.5) + rnorm(500, 0, 0.5) + rnorm(500, 0, 0.5) + rnorm(500, 0, rexp(1,2)) + rnorm(500, 0, rexp(1,1))
slope_Ty <- rnorm(500, 0, 0.5) + rnorm(500, 0, 0.5) + rnorm(500, 0, 0.5) + rnorm(500, 0, rexp(1,2)) + rnorm(500, 0, rexp(1,1))

slopes_all = list(slope_B, slope_R, 
                  slope_S, slope_T,
                  slope_Tk, slope_Tb, slope_Ty)

graphics.off()

pdf("../figures/figure2.pdf", width = 12.5)
par(mfrow= c(1, 2), oma = c(4,3,4,0), mar = c(0.5, 2, 0.2, 3))

## width and space values for the bar plot bins (will be needed for later)
bplot_width = 1
bplot_space = 0.2

plot(1,1,col="white", xlim = c(0,5), ylim = c(0,1), axes = FALSE, ylab = "Probability", xlab = "")
grid(nx = 0, ny = NULL)
## plot a bar plot of raw data
barplot(prop.table(table(data$kidnap, data$ethnic_group), margin = 2)[2,], beside = T,
        width = bplot_width, space = bplot_space, xlim = c(0,5), ylim = c(0,1), add = T,
        names.arg = c("Borana", "Rendille", "Samburu", "Turkana"), col = palette_Paired[2])

for(i in 0:3){
  
  ## convert estimated logit values to probabilities
  p = logit2prob(slopes_all[[i+1]])

  ## formula to position the prior intervals on the x-axis
  pos_x = bplot_width*(i+0.5) + bplot_space*(i+1)
  
  rect(xleft = pos_x-0.05, ybottom = hdi(p)[1],
       xright = pos_x+0.05, ytop = hdi(p)[2],
       lwd = 1, col = palette_Dark2[6], border = "black")  
  # segments(x0 = pos_x, y0 = quantile(y_all[[i+1]], 0.025),
  #          x1 = pos_x, y1 = quantile(y_all[[i+1]], 0.975),
  #          lwd = 3, col = "black")
  lines(x = c(pos_x-0.05, pos_x+0.05), y = rep(median(p),2), lwd = 2, lty = 1)
}

plot(1,1,col="white", xlim = c(0,5), ylim = c(0,1), axes = F, xlab = "")
grid(nx = 0, ny = NULL)
barplot(prop.table(table(data$kidnap, data$territorial_section)[,2:4], margin = 2)[2,], beside = T,
        width = bplot_width, space = bplot_space, xlim = c(0,5), add = T,
        names.arg = c("Kwatela", "Ngibochoros", "Ngiyapakuno"), col = palette_Paired[2], ylim = c(0,1))

for(i in 0:2){
  
  ## convert estimated logit values to probabilities
  p = logit2prob(slopes_all[[i+4]])
  
  ## formula to position the prior intervals on the x-axis
  pos_x = bplot_width*(i+0.5) + bplot_space*(i+1)
  
  rect(xleft = pos_x-0.05, ybottom = hdi(p)[1],
       xright = pos_x+0.05, ytop = hdi(p)[2],
       lwd = 1, col = palette_Dark2[6], border = "black")  
  # segments(x0 = pos_x, y0 = quantile(y_all[[i+1]], 0.025),
  #          x1 = pos_x, y1 = quantile(y_all[[i+1]], 0.975),
  #          lwd = 3, col = "black")
  lines(x = c(pos_x-0.05, pos_x+0.05), y = rep(median(p),2), lwd = 2, lty = 1)
}

title(ylab = "p(DISAGREE)", outer = TRUE, line = 1.5)
par(xpd=NA)
text(-7.45, 1.095, "a", cex = 2)
text(-1, 1.1, "b", cex = 2)
## reset graphics parameters
dev.off()

## --------------------------------
## POSTERIOR PREDICTIVE CHECK
## --------------------------------

pB_full_model = logit2prob(b0_Borana + sex_effect + school_yrs_effect + town_yrs_effect +
                             ms_divorced_effect + ms_married_effect + ms_single_effect + 
                             ms_unofficial_effect + ms_widowed_effect)

pR_full_model = logit2prob(b0_Rendille + sex_effect + school_yrs_effect + town_yrs_effect +
                             ms_divorced_effect + ms_married_effect + ms_single_effect + 
                             ms_unofficial_effect + ms_widowed_effect)

pS_full_model = logit2prob(b0_Samburu + sex_effect + school_yrs_effect + town_yrs_effect +
                             ms_divorced_effect + ms_married_effect + ms_single_effect + 
                             ms_unofficial_effect + ms_widowed_effect)

pT_full_model = logit2prob(b0_Turkana + sex_effect + school_yrs_effect + town_yrs_effect +
                             ms_divorced_effect + ms_married_effect + ms_single_effect + 
                             ms_unofficial_effect + ms_widowed_effect)

pTk_full_model = logit2prob(b0_Turkana + sex_effect + school_yrs_effect + town_yrs_effect +
                              ms_divorced_effect + ms_married_effect + ms_single_effect + 
                              ms_unofficial_effect + ms_widowed_effect + ts_kwatela_effect)

pTb_full_model = logit2prob(b0_Turkana + sex_effect + school_yrs_effect + town_yrs_effect +
                              ms_divorced_effect + ms_married_effect + ms_single_effect + 
                              ms_unofficial_effect + ms_widowed_effect + ts_ngibochoros_effect)

pTy_full_model = logit2prob(b0_Turkana + sex_effect + school_yrs_effect + town_yrs_effect +
                              ms_divorced_effect + ms_married_effect + ms_single_effect + 
                              ms_unofficial_effect + ms_widowed_effect + ts_ngiyapakuno_effect)

p_full_model_all = list(pB_full_model,
                        pR_full_model,
                        pS_full_model,
                        pT_full_model,
                        pTk_full_model,
                        pTb_full_model,
                        pTy_full_model)

graphics.off()

pdf("../figures/figure3.pdf", width = 12.5)
par(mfrow= c(1, 2), oma = c(4,3,4,0), mar = c(0.5, 2, 0.2, 3))

#par(mfrow= c(1, 2), oma = c(4,5,4,1), mar = c(0.5, 0.5, 0.2, 2))
plot(1,1,col="white", xlim = c(0,5), ylim = c(0,1), axes = F, ylab = "Probability", xlab = "")
grid(nx = 0, ny = NULL)
#par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(prop.table(table(data$kidnap, data$ethnic_group), margin = 2)[2,], beside = T,
        width = bplot_width, space = bplot_space, xlim = c(0,5), ylim = c(0,1),
        names.arg = c("Borana", "Rendille", "Samburu", "Turkana"), col = palette_Paired[2], add = T)
title(ylab = "p(Disagree)", outer = TRUE)

for(i in 0:3){
  
  ## formula to position the prior intervals on the x-axis
  pos_x = bplot_width*(i+0.65) + bplot_space*(i+1.15)
  
  rect(xleft = pos_x-0.05, ybottom = hdi(p_full_model_all[[i+1]])[1],
       xright = pos_x+0.05, ytop = hdi(p_full_model_all[[i+1]])[2],
       lwd = 1, col = palette_Dark2[2], border = "black")  
  # segments(x0 = pos_x, y0 = quantile(y_all[[i+1]], 0.025),
  #          x1 = pos_x, y1 = quantile(y_all[[i+1]], 0.975),
  #          lwd = 3, col = "black")
  lines(x = c(pos_x-0.05, pos_x+0.05), y = rep(median(p_full_model_all[[i+1]]),2), lwd = 2, lty = 1)
  #points(pos_x, median(p_full_model_all[[i+1]]), col = "black", bg = palette_Dark2[2], pch = 21, cex = 2, lwd = 1)
}

for(i in 0:3){
  
  ## convert estimated logit values to probabilities
  p = logit2prob(slopes_all[[i+1]])
  
  ## formula to position the prior intervals on the x-axis
  pos_x = bplot_width*(i+0.35) + bplot_space*(i+0.85)
  
  rect(xleft = pos_x-0.05, ybottom = hdi(p)[1],
       xright = pos_x+0.05, ytop = hdi(p)[2],
       lwd = 1, col = palette_Dark2[6], border = "black")  
  lines(x = c(pos_x-0.05, pos_x+0.05), y = rep(median(p),2), lwd = 2, lty = 1)
}

#per territorial section
plot(1,1,col="white", xlim = c(0,5), ylim = c(0,1), axes = F, ylab = "Probability", xlab = "")
grid(nx = 0, ny = NULL)
barplot(prop.table(table(data$kidnap, data$territorial_section)[,2:4], margin = 2)[2,], beside = T,
        width = bplot_width, space = bplot_space, xlim = c(0,5),
        names.arg = c("Kwatela", "Ngibochoros", "Ngiyapakuno"), col = palette_Paired[2], ylim = c(0,1), add = T)
# legend("topright", legend = c("Agree", "Disagree"), fill = col_palette[c(2,6)])

for(i in 0:2){
  
  ## formula to position the prior intervals on the x-axis
  pos_x = bplot_width*(i+0.65) + bplot_space*(i+1.15)
  
  rect(xleft = pos_x-0.05, ybottom = hdi(p_full_model_all[[i+4]])[1],
       xright = pos_x+0.05, ytop = hdi(p_full_model_all[[i+4]])[2],
       lwd = 1, col = palette_Dark2[2], border = "black")  
  # segments(x0 = pos_x, y0 = quantile(y_all[[i+1]], 0.025),
  #          x1 = pos_x, y1 = quantile(y_all[[i+1]], 0.975),
  #          lwd = 3, col = "black")
  lines(x = c(pos_x-0.05, pos_x+0.05), y = rep(median(p_full_model_all[[i+4]]),2), lwd = 2, lty = 1)
  #points(pos_x, median(p_full_model_all[[i+1]]), col = "black", bg = palette_Dark2[2], pch = 21, cex = 2, lwd = 1)
}

for(i in 0:2){
  
  ## convert estimated logit values to probabilities
  p = logit2prob(slopes_all[[i+1]])
  
  ## formula to position the prior intervals on the x-axis
  pos_x = bplot_width*(i+0.35) + bplot_space*(i+0.85)
  
  rect(xleft = pos_x-0.05, ybottom = hdi(p)[1],
       xright = pos_x+0.05, ytop = hdi(p)[2],
       lwd = 1, col = palette_Dark2[6], border = "black")  
  lines(x = c(pos_x-0.05, pos_x+0.05), y = rep(median(p),2), lwd = 2, lty = 1)
}

title(ylab = "p(DISAGREE)", outer = TRUE, line = 1.5)
par(xpd=NA)
text(-7.45, 1.095, "a", cex = 2)
text(-1, 1.1, "b", cex = 2)

## reset graphics parameters
dev.off()

## --------------------------------
## FULL MODEL (WITH EFFECTS)
## --------------------------------

## rgb values of palette I'm using
## 1 - 27,158,119
## 2 - 217,95,2
## 3 - 117,112,179
## 6 - 230,171,2

graphics.off()

pdf("../figures/figure4.pdf")
par(mfrow=c(2,1), oma = c(2,2,2,2), mar = c(3, 1, 3, 1))

#histogram of baselines
pB = logit2prob(b0_Borana)
hist(pB, col = rgb(27, 158, 119, alpha = 100, maxColorValue = 255), probability = T, breaks = seq(from=0, to=1, by=0.02),
     xlab = "", main = "", xlim = c(0,1), ylim = c(0, 15))
#histogram of baselines
pR = logit2prob(b0_Rendille)
hist(pR, col = rgb(217, 95, 2, alpha = 100, maxColorValue = 255), probability = T, breaks = seq(from=0, to=1, by=0.02), add = T)
#histogram of baselines
pS = logit2prob(b0_Samburu)
hist(pS, col = rgb(117, 112, 179, alpha = 100, maxColorValue = 255), probability = T, breaks = seq(from=0, to=1, by=0.02), add = T)
#histogram of baselines
pT = logit2prob(b0_Turkana)
hist(pT, col = rgb(230, 171, 2, alpha = 100, maxColorValue = 255), probability = T, breaks = seq(from=0, to=1, by=0.02), add = T)

#histogram of full model with effects
hist(pB_full_model, col = rgb(27, 158, 119, alpha = 200, maxColorValue = 255), probability = T, breaks = seq(from=0, to=1, by=0.02), add = T)
hist(pR_full_model, col = rgb(217, 95, 2, alpha = 200, maxColorValue = 255), probability = T, breaks = seq(from=0, to=1, by=0.02), add = T)
hist(pS_full_model, col = rgb(117, 112, 179, alpha = 200, maxColorValue = 255), probability = T, breaks = seq(from=0, to=1, by=0.02), add = T)
hist(pT_full_model, col = rgb(230, 171, 2, alpha = 200, maxColorValue = 255), probability = T, breaks = seq(from=0, to=1, by=0.02), add = T)


legend("topleft", legend = c("Borana", "Rendille", "Samburu", "Turkana"), fill = palette_Dark2[c(1:3,6)],
       bty = "n", xpd = TRUE, cex = 0.8, inset = c(0.25,-0.025))

par(xpd=NA)
text(0, 18, "a", cex = 1.5)

#histograms by territorial sections

pTk = logit2prob(b0_Turkana+ts_kwatela_effect)
pTb = logit2prob(b0_Turkana+ts_ngibochoros_effect)
pTy = logit2prob(b0_Turkana+ts_ngiyapakuno_effect)

hist(pTk, col = rgb(col2rgb(palette_Set1[1])[1,],
                    col2rgb(palette_Set1[1])[2,],
                    col2rgb(palette_Set1[1])[3,], alpha = 100, maxColorValue = 255), probability = T, breaks = seq(from=0, to=1, by=0.02),
     xlab = "", main = "", xlim = c(0,1), ylim = c(0, 15))
hist(pTb, col = rgb(col2rgb(palette_Set1[2])[1,],
                    col2rgb(palette_Set1[2])[2,],
                    col2rgb(palette_Set1[2])[3,], alpha = 100, maxColorValue = 255), probability = T, breaks = seq(from=0, to=1, by=0.02), add = T)
hist(pTy, col = rgb(col2rgb(palette_Set1[3])[1,],
                    col2rgb(palette_Set1[3])[2,],
                    col2rgb(palette_Set1[3])[3,], 2, alpha = 100, maxColorValue = 255), probability = T, breaks = seq(from=0, to=1, by=0.02), add = T)

hist(pTk_full_model, col = rgb(col2rgb(palette_Set1[1])[1,],
                               col2rgb(palette_Set1[1])[2,],
                               col2rgb(palette_Set1[1])[3,], alpha = 200, maxColorValue = 255), probability = T, breaks = seq(from=0, to=1, by=0.02), add = T)
hist(pTb_full_model, col = rgb(col2rgb(palette_Set1[2])[1,],
                               col2rgb(palette_Set1[2])[2,],
                               col2rgb(palette_Set1[2])[3,], alpha = 200, maxColorValue = 255), probability = T, breaks = seq(from=0, to=1, by=0.02), add = T)
hist(pTy_full_model, col = rgb(col2rgb(palette_Set1[3])[1,],
                               col2rgb(palette_Set1[3])[2,],
                               col2rgb(palette_Set1[3])[3,], alpha = 200, maxColorValue = 255), probability = T, breaks = seq(from=0, to=1, by=0.02), add = T)

legend("topleft", legend = c("Kwatela", "Ngibochoros", "Ngiyapakuno"), fill = palette_Set1[c(1:3)],
       bty = "n", xpd = TRUE, cex = 0.8, inset = c(0.25,-0.025))

title(xlab = "p(DISAGREE)", outer = TRUE, line = 0.25)
par(xpd=NA)
text(0, 18, "b", cex = 1.5)

dev.off()

# mean_pB = mean(pB)
# mean_pR = mean(pR)
# mean_pS = mean(pS)
# mean_pT = mean(pT)
# 
# mean_p = c(mean_pB, mean_pR, mean_pS, mean_pT)

#plot raw data
#x=aggregate(data_list$kidnap, list(data_list$ethnic_group), mean)[[1]]
# y=aggregate(data_list$kidnap, list(data_list$ethnic_group), mean)[[2]]
#plot(y~x, bty="n", pch=16, col="aquamarine3", ylim = c(0,1), xlab = "Ethnic Group",
#     ylab = "mean(p[i])")
#lines(mean_p~x, type = "p")

#densplot(samples[1:3][,1], lwd = 2, col = "maroon", bty = "n")

#densplot(mcmc.list(mcmc(exp(samples[[1]][,1])/(1+exp(samples[[1]][,1]))),
#                   mcmc(exp(samples[[2]][,1])/(1+exp(samples[[2]][,1]))),
#                   mcmc(exp(samples[[3]][,1])/(1+exp(samples[[3]][,1])))), lwd=2, col = "maroon", bty="n", xlim = c(0,1))

# hdi(pB)
# summary(pB)
# hdi(pR)
# summary(pR)
# hdi(pS)
# summary(pS)
# hdi(pT)
# summary(pT)

#boxplot(pB, pR, pS, pT, ylim = c(0,1), names = c("Borana", "Rendille", "Samburu", "Turkana"), col = col_palette[c(1:3,6)],
#        horizontal = FALSE, width = sqrt(as.numeric(table(data_list$ethnic_group))))
#raw average probability
#points(1:4, y, pch = 21, col = "black", bg = "maroon")
#estimated average probabilities
#points(1:4, mean_p, pch = 21, col = c("black"), bg = "white")

# install.packages("vioplot")
# library("vioplot")
# 
# estimated_samples = data.frame(p=c(pB,pR,pS,pT), ethnic_group=rep(1:4, each = length(pB)))
# vioplot(estimated_samples$p~estimated_samples$ethnic_group, ylim = c(0,1), col = palette_Dark2[c(1:3,6)], trim = FALSE,
#         horizontal = FALSE, plotCentre = "line", names = c("Borana", "Rendille", "Samburu", "Turkana"), #side = "right",
#         xlab = "Ethnic Group", ylab = "Density")
# #raw average probability
# points(1:4, y, pch = 21, col = "black", bg = "maroon", cex = 2)
# #estimated average probabilities
# points(1:4, mean_p, pch = 21, col = c("black"), bg = "white", cex = 1.5)

#legend(1.5,1, pch = c(21,16), col = c("black", "maroon"), bg = "white", legend = c("Estimated Mean", "Raw Mean"))

#hist(pB_full_model, probability = TRUE, main = "", xlab = "", ylab = "")
#axis(1)
#lines(density(pB_full_model), lwd = 2, col = "red")
#par(new=TRUE)
#vioplot(polygon(pB_full_model), horizontal = TRUE, yaxt = "n", axes = FALSE, col = rgb(0,1,1,0.1))


## --------------------------------
## SUMMARIZING POSTERIOR OF EFFECTS
## --------------------------------

graphics.off()

pdf("../figures/figure5.pdf")
#par(oma = c(2,5,2,5), mar = c(2.5, 1, 1, 1))
par(oma = c(2,4,1,2), mar = c(4, 4, 3, 5))
#summary of all effects
y_labels=c("Sex", "School Years", "Town Years", "Divorced", "Married", "Single", "Unofficial", "Widowed", "No Territory", "Kwatela", "Ngibochoros", "Ngiyapakuno")

estimated_effects=list(sex_effect, school_yrs_effect, town_yrs_effect, ms_divorced_effect, ms_married_effect, ms_single_effect, ms_unofficial_effect,
  ms_widowed_effect, no_ts_effect, ts_kwatela_effect, ts_ngibochoros_effect, ts_ngiyapakuno_effect)

plot(1,1, xlim = c(-1.5,1.5), ylim = c(1,12), pch = 21, col = "white", bty = "n", ylab = "", xlab = "logit(effect)", yaxt = "n")
axis(side = 2, at = 1:12, labels = y_labels, las = 1)
abline(v=0, col = palette_Paired[6])

for(i in 1:length(estimated_effects)){
  rect(xleft = hdi(estimated_effects[[i]])[1], 
       ybottom = i-0.075,
       xright = hdi(estimated_effects[[i]])[2],
       ytop = i+0.075,
       col = palette_Paired[10], border = "black")
  rect(xleft = hdi(estimated_effects[[i]], 0.5)[1], 
       ybottom = i-0.075,
       xright = hdi(estimated_effects[[i]], 0.5)[2],
       ytop = i+0.075,
       col = palette_Paired[9], border = "black")
  rect(xleft = median(estimated_effects[[i]])-0.003,
       ybottom = i-0.075,
       xright = median(estimated_effects[[i]])+0.003,
       ytop = i+0.075,
       lwd = 1, col = palette_Set1[5], border = "black")
  # rect(xleft = mean(estimated_effects[[i]])-0.003,
  #      ybottom = i-0.075,
  #      xright = mean(estimated_effects[[i]])+0.003,
  #      ytop = i+0.075,
  #      lwd = 1, col = palette_Set1[6], border = "black")
}

legend("topleft", legend = c("50% HDI", "95% HDI"), fill = palette_Paired[9:10],
       bty = "n", xpd = TRUE, cex = 0.8, inset = c(0.935,0.4325))

dev.off()
## --------------------------------
## EXPLORING SIGNIFICANT EFFECTS
## --------------------------------

# graphics.off()
# #beta1 - sex has a statistically significant (95%) NEGATIVE effect
# hist(pB, ylim = c(0, 5000))
pB_with_sex_effect = logit2prob(b0_Borana+sex_effect)
# hist(pB_with_sex_effect, col=NULL, border = "red", add = T)
# 
# hist(pR, ylim = c(0, 5000))
pR_with_sex_effect = logit2prob(b0_Rendille+sex_effect)
# hist(pR_with_sex_effect, col=NULL, border = "red", add = T)
# 
# hist(pS, ylim = c(0, 5000))
pS_with_sex_effect = logit2prob(b0_Samburu+sex_effect)
# hist(pS_with_sex_effect, col=NULL, border = "red", add = T)
# 
# hist(pT, ylim = c(0, 5000))
pT_with_sex_effect = logit2prob(b0_Turkana+sex_effect)
# hist(pT_with_sex_effect, col=NULL, border = "red", add = T)
#effect seems minimal
#show by how much does p increase given the effect of town years
hdi(pB_with_sex_effect-pB)
summary(pB_with_sex_effect-pB)
# hist(pB_with_sex_effect-pB, probability = T, xlim = c(-0.3,0.1), col = NULL, border = rgb(27, 158, 119, maxColorValue = 255))
# lines(density(pB_with_sex_effect-pB), lwd = 2, col = col_palette[1])
# 
hdi(pR_with_sex_effect-pR)
summary(pR_with_sex_effect-pR)
# hist(pR_with_sex_effect-pR, probability = T, add = T, col = NULL, border = rgb(217, 95, 2, maxColorValue = 255))
# lines(density(pR_with_sex_effect-pR), lwd = 2, col = col_palette[2])
# 
hdi(pS_with_sex_effect-pS)
summary(pS_with_sex_effect-pS)
# hist(pS_with_sex_effect-pS, probability = T, add = T, col = NULL, border = rgb(117, 112, 179, maxColorValue = 255))
# lines(density(pS_with_sex_effect-pS), lwd = 2, col = col_palette[3])
# 
hdi(pT_with_sex_effect-pT)
summary(pT_with_sex_effect-pT)
# hist(pT_with_sex_effect-pT, probability = T, add = T, col = NULL, border = rgb(230, 171, 2, maxColorValue = 255))
# lines(density(pT_with_sex_effect-pT), lwd = 2, col = col_palette[6])
# 
# legend("topright", legend = c("Borana", "Rendille", "Samburu", "Turkana"), fill = col_palette[c(1:3,6)])
# ########################################################################################################################
# # SEX EFFECT ON NORM OUTCOME PER ETHNIC GROUP
# ########################################################################################################################
# female_estimated_samples = estimated_samples
# male_estimated_samples = data.frame(p=c(pB_with_sex_effect,pR_with_sex_effect,pS_with_sex_effect,pT_with_sex_effect), 
#                                     ethnic_group=rep(1:4, each = length(pB_with_sex_effect)))
# 
# palette_Paired = brewer.pal(12, "Paired")
# 
# vioplot(female_estimated_samples$p~female_estimated_samples$ethnic_group, ylim = c(0,1), col = palette_Paired[1], 
#         horizontal = FALSE, plotCentre = "line", names = c("Borana", "Rendille", "Samburu", "Turkana"), side = "left",
#         xlab = "Ethnic Group", ylab = "Probability", areaEqual = T,
#         panel.first = c(abline(h=0.5, lty="dashed", lwd=2, col = rgb(255,0,0, alpha = 125, maxColorValue = 255)), 
#                         grid(nx = 0, ny = NULL, col = rgb(211,211,211, alpha = 100, maxColorValue = 255), 
#                            lwd = 2, lty = "solid")))
# vioplot(male_estimated_samples$p~male_estimated_samples$ethnic_group, col = palette_Paired[2], side = "right",
#         plotCentre = "line", areaEqual = T, add = TRUE)
# 
# 
# raw_mean_female = aggregate(data_list$kidnap, list(data_list$ethnic_group, data_list$sex), mean)[1:4,3]
# raw_mean_male = aggregate(data_list$kidnap, list(data_list$ethnic_group, data_list$sex), mean)[5:8,3]
# #raw average probability
# points(1:4, raw_mean_female, pch = 21, col = "black", bg = "white", cex = 1.5)
# points(1:4, raw_mean_male, pch = 23, col = "black", bg = "white", cex = 1.5)
# 
# mean_p_with_sex_effect = c(mean(pB_with_sex_effect), mean(pR_with_sex_effect), 
#                            mean(pS_with_sex_effect), mean(pT_with_sex_effect))
# #estimated average probabilities
# points(1:4, mean_p, pch = 21, col = c("black"), bg = palette_Paired[11], cex = 1.25)
# points(1:4, mean_p_with_sex_effect, pch = 23, col = c("black"), bg = palette_Paired[12], cex = 1.25)
# 
# legend_coords=legend("topright", plot = FALSE,
#        legend = c("Female", "Male", "Estimated Mean (F)", "Estimated Mean (M)", "Raw Mean (F)", "Raw Mean (M)"), 
#        pch = c(22,22,21,23,21,23), col = "black", pt.cex = c(2,2,1.25,1.25,1.5,1.5),
#        pt.bg = c(palette_Paired[c(1,2,11,12)], "white", "white"))
# 
# legend(x = c(0.341, 1.4), y = c(legend_coords$rect$top, 0.65),
#        legend = c("Female", "Male", "Estimated Mean (F)", "Estimated Mean (M)", "Raw Mean (F)", "Raw Mean (M)"), 
#        pch = c(22,22,21,23,21,23), col = "black", pt.cex = c(2,2,1.5,1.5,1,1),
#        pt.bg = c(palette_Paired[c(1,2,11,12)], "white", "white"))

#beta2 - number of years of schooling has a statistically significant (95%) POSITIVE effect
# hist(pB, ylim = c(0, 5000))
pB_with_school_yrs_effect = logit2prob(b0_Borana+school_yrs_effect)
# hist(pB_with_school_yrs_effect, col=NULL, border = "red", add = T)
# 
# hist(pR, ylim = c(0, 5000))
pR_with_school_yrs_effect = logit2prob(b0_Rendille+school_yrs_effect)
# hist(pR_with_school_yrs_effect, col=NULL, border = "red", add = T)
# 
# hist(pS, ylim = c(0, 5000))
pS_with_school_yrs_effect = logit2prob(b0_Samburu+school_yrs_effect)
# hist(pS_with_school_yrs_effect, col=NULL, border = "red", add = T)
# 
# hist(pT, ylim = c(0, 5000))
pT_with_school_yrs_effect = logit2prob(b0_Turkana+school_yrs_effect)
# hist(pT_with_school_yrs_effect, col=NULL, border = "red", add = T)
# #effect seems minimal
# #show by how much does p increase given the effect of town years
hdi(pB_with_school_yrs_effect-pB)
summary(pB_with_school_yrs_effect-pB)
# hist(pB_with_school_yrs_effect-pB, probability = T, xlim = c(-0.1,0.1), col = NULL, border = rgb(27, 158, 119, maxColorValue = 255))
# lines(density(pB_with_school_yrs_effect-pB), lwd = 2, col = col_palette[1])
# 
hdi(pR_with_school_yrs_effect-pR)
summary(pR_with_school_yrs_effect-pR)
# hist(pR_with_school_yrs_effect-pR, probability = T, add = T, col = NULL, border = rgb(217, 95, 2, maxColorValue = 255))
# lines(density(pR_with_school_yrs_effect-pR), lwd = 2, col = col_palette[2])
# 
hdi(pS_with_school_yrs_effect-pS)
summary(pS_with_school_yrs_effect-pS)
# hist(pS_with_school_yrs_effect-pS, probability = T, add = T, col = NULL, border = rgb(117, 112, 179, maxColorValue = 255))
# lines(density(pS_with_school_yrs_effect-pS), lwd = 2, col = col_palette[3])
# 
hdi(pT_with_school_yrs_effect-pT)
summary(pT_with_school_yrs_effect-pT)
# hist(pT_with_school_yrs_effect-pT, probability = T, add = T, col = NULL, border = rgb(230, 171, 2, maxColorValue = 255))
# lines(density(pT_with_school_yrs_effect-pT), lwd = 2, col = col_palette[6])
# 
# legend("topright", legend = c("Borana", "Rendille", "Samburu", "Turkana"), fill = col_palette[c(1:3,6)])
# ########################################################################################################################
# # SEX EFFECT ON NORM OUTCOME PER ETHNIC GROUP
# ########################################################################################################################
# female_estimated_samples = estimated_samples
# male_estimated_samples = data.frame(p=c(pB_with_sex_effect,pR_with_sex_effect,pS_with_sex_effect,pT_with_sex_effect), 
#                                     ethnic_group=rep(1:4, each = length(pB_with_sex_effect)))
# 
# palette_Paired = brewer.pal(12, "Paired")
# 
# vioplot(female_estimated_samples$p~female_estimated_samples$ethnic_group, ylim = c(0,1), col = palette_Paired[1], 
#         horizontal = FALSE, plotCentre = "line", names = c("Borana", "Rendille", "Samburu", "Turkana"), side = "left",
#         xlab = "Ethnic Group", ylab = "Probability", 
#         panel.first = c(abline(h=0.5, lty="dashed", lwd=2, col = rgb(255,0,0, alpha = 125, maxColorValue = 255)), 
#                         grid(nx = 0, ny = NULL, col = rgb(211,211,211, alpha = 100, maxColorValue = 255), 
#                              lwd = 2, lty = "solid")))
# vioplot(male_estimated_samples$p~male_estimated_samples$ethnic_group, col = palette_Paired[2], side = "right",
#         plotCentre = "line", add = TRUE)
# 
# 
# raw_mean_female = aggregate(data_list$kidnap, list(data_list$ethnic_group, data_list$sex), mean)[1:4,3]
# raw_mean_male = aggregate(data_list$kidnap, list(data_list$ethnic_group, data_list$sex), mean)[5:8,3]
# #raw average probability
# points(1:4, raw_mean_female, pch = 21, col = "black", bg = "white", cex = 1.5)
# points(1:4, raw_mean_male, pch = 23, col = "black", bg = "white", cex = 1.5)
# 
# mean_p_with_sex_effect = c(mean(pB_with_sex_effect), mean(pR_with_sex_effect), 
#                            mean(pS_with_sex_effect), mean(pT_with_sex_effect))
# #estimated average probabilities
# points(1:4, mean_p, pch = 21, col = c("black"), bg = palette_Paired[11], cex = 1.25)
# points(1:4, mean_p_with_sex_effect, pch = 23, col = c("black"), bg = palette_Paired[12], cex = 1.25)
# 
# legend_coords=legend("topright", plot = FALSE,
#                      legend = c("Female", "Male", "Estimated Mean (F)", "Estimated Mean (M)", "Raw Mean (F)", "Raw Mean (M)"), 
#                      pch = c(22,22,21,23,21,23), col = "black", pt.cex = c(2,2,1.25,1.25,1.5,1.5),
#                      pt.bg = c(palette_Paired[c(1,2,11,12)], "white", "white"))
# 
# legend(x = c(0.341, 1.4), y = c(legend_coords$rect$top, 0.65),
#        legend = c("Female", "Male", "Estimated Mean (F)", "Estimated Mean (M)", "Raw Mean (F)", "Raw Mean (M)"), 
#        pch = c(22,22,21,23,21,23), col = "black", pt.cex = c(2,2,1.5,1.5,1,1),
#        pt.bg = c(palette_Paired[c(1,2,11,12)], "white", "white"))
# 
# hist(pB, col = NULL)
# hist(pB_with_sex_effect, add=T, col = NULL, border="red")
# hist(pB_with_school_yrs_effect, add = T, col = NULL, border="green")
# pB_with_sex_school_yrs_effect=exp(b0_Borana+sex_effect+school_yrs_effect)/(1+exp(b0_Borana+sex_effect+school_yrs_effect))
# hist(pB_with_sex_school_yrs_effect, add = T, col = NULL, border="blue")

#sex effect per territorial section
pTk_with_sex_effect = logit2prob(b0_Turkana+ts_kwatela_effect+sex_effect)
pTb_with_sex_effect = logit2prob(b0_Turkana+ts_ngibochoros_effect+sex_effect)
pTy_with_sex_effect = logit2prob(b0_Turkana+ts_ngiyapakuno_effect+sex_effect)

hdi(pTk_with_sex_effect-pTk)
summary(pTk_with_sex_effect-pTk)
hdi(pTb_with_sex_effect-pTb)
summary(pTb_with_sex_effect-pTb)
hdi(pTy_with_sex_effect-pTy)
summary(pTy_with_sex_effect-pTy)

#school year effect per territorial section
pTk_with_school_yrs_effect = logit2prob(b0_Turkana+ts_kwatela_effect+school_yrs_effect)
pTb_with_school_yrs_effect = logit2prob(b0_Turkana+ts_ngibochoros_effect+school_yrs_effect)
pTy_with_school_yrs_effect = logit2prob(b0_Turkana+ts_ngiyapakuno_effect+school_yrs_effect)

hdi(pTk_with_school_yrs_effect-pTk)
summary(pTk_with_school_yrs_effect-pTk)
hdi(pTb_with_school_yrs_effect-pTb)
summary(pTb_with_school_yrs_effect-pTb)
hdi(pTy_with_school_yrs_effect-pTy)
summary(pTy_with_school_yrs_effect-pTy)

## average magnitudes of significant effects

graphics.off()

pdf("../figures/figure6.pdf")
par(oma = c(3,2.5,2,2), mar = c(2, 2, 2, 3))
hist(logit2prob(sex_effect), probability = T, xlim = c(0.2,0.6), ylim = c(0,40), col = palette_Set1[2],
     xlab = "Probability", main = "")


lines(density(logit2prob(sex_effect)), lwd = 2, col = "maroon")
lines(x = rep(hdi(logit2prob(sex_effect))[1], 2), y = c(0, 40), 
      lwd = 2, col = "black", lty = 3)
lines(x = rep(hdi(logit2prob(sex_effect))[2], 2), y = c(0, 40),
      lwd = 2, col = "black", lty = 3)
lines(x = rep(median(logit2prob(sex_effect)), 2), y = c(0, 40),
      lwd = 2, col = "black", lty = 2)

hist(logit2prob(school_yrs_effect), probability = T, col = palette_Set1[3], add = T)
     #breaks = seq(from=0, to=1, by=0.005), add = T)
lines(density(logit2prob(school_yrs_effect)), lwd = 2, col = "maroon")
lines(x = rep(hdi(logit2prob(school_yrs_effect))[1], 2), y = c(0, 40), 
      lwd = 2, col = "black", lty = 3)
lines(x = rep(hdi(logit2prob(school_yrs_effect))[2], 2), y = c(0, 40),
      lwd = 2, col = "black", lty = 3)
lines(x = rep(median(logit2prob(school_yrs_effect)), 2), y = c(0, 40),
      lwd = 2, col = "black", lty = 2)

legend("topleft", legend = c("Sex", "School Year"), fill = palette_Set1[c(2:3)],
       bty = "n", xpd = NA, cex = 0.8, inset = c(0.95,0.42), 
       title = "Magnitude effect", title.adj = 1)

title(xlab = "Probability", outer = TRUE, line = 1)
title(ylab = "Density", outer = TRUE, line = 1)

dev.off()

