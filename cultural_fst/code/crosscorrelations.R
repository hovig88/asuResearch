chain_cors = crosscorr(samples)
chain_cors[!lower.tri(chain_cors)] <- 0
autocorr_degree = c(5:0)

chain_cors_neg = chain_cors < 0

chain_cors[chain_cors >= 0.8] <- autocorr_degree[1]
chain_cors[chain_cors >= 0.6 & chain_cors < 0.8] <- autocorr_degree[2]
chain_cors[chain_cors >= 0.4 & chain_cors < 0.6] <- autocorr_degree[3]
chain_cors[chain_cors >= 0.2 & chain_cors < 0.4] <- autocorr_degree[4]
chain_cors[chain_cors > 0 & chain_cors < 0.2] <- autocorr_degree[5]
chain_cors[chain_cors == 0] <- autocorr_degree[6]
chain_cors[chain_cors > -0.2 & chain_cors < 0] <- autocorr_degree[5]
chain_cors[chain_cors > -0.4 & chain_cors <= -0.2] <- autocorr_degree[4]
chain_cors[chain_cors > -0.6 & chain_cors <= -0.4] <- autocorr_degree[3]
chain_cors[chain_cors > -0.8 & chain_cors <= -0.6] <- autocorr_degree[2]
chain_cors[chain_cors <= -0.8] <- autocorr_degree[1]

chain_cors[chain_cors_neg] <- chain_cors[chain_cors_neg]*(-1)

autocorr_deg_inds = data.frame()
for(i in 1:length(autocorr_degree)){
  if(length(chain_cors[chain_cors == autocorr_degree[i]])>0){
    autocorr_deg_inds = rbind(autocorr_deg_inds,
                              cbind(dims = which(chain_cors==chain_cors[chain_cors == autocorr_degree[i]], arr.ind = T), 
                                    degree = autocorr_degree[i],
                                    row.param = rownames(chain_cors)[which(chain_cors==chain_cors[chain_cors == autocorr_degree[i]], arr.ind = T)[,1]],
                                    col.param = colnames(chain_cors)[which(chain_cors==chain_cors[chain_cors == autocorr_degree[i]], arr.ind = T)[,2]]))
  }
  if(length(chain_cors[chain_cors == -autocorr_degree[i]])>0){
    autocorr_deg_inds = rbind(autocorr_deg_inds,
                              cbind(dims = which(chain_cors==chain_cors[chain_cors == -autocorr_degree[i]], arr.ind = T), 
                                    degree = -autocorr_degree[i],
                                    row.param = rownames(chain_cors)[which(chain_cors==chain_cors[chain_cors == -autocorr_degree[i]], arr.ind = T)[,1]],
                                    col.param = colnames(chain_cors)[which(chain_cors==chain_cors[chain_cors == -autocorr_degree[i]], arr.ind = T)[,2]]))
  }
}

rownames(autocorr_deg_inds)=1:dim(autocorr_deg_inds)[1]

#exclude very low crosscorrelations (between -0.2 and 0.2)
high_cors = autocorr_deg_inds[!(autocorr_deg_inds$degree %in% c(0,1,-1)),]

#separate the crosscorrelations based on their relative degrees
beta_cors = data.frame()
clan_cors = data.frame()
ethnic_group_cors = data.frame()
marital_status_cors = data.frame()
sd_cors = data.frame()
sd_params = c("sd_clan", "sd_ethnic_group", "sd_marital_status", "sd_subclan", "sd_territorial_section")
subclan_cors = data.frame()
territorial_section_cors = data.frame()

for(j in c(1,49)){
  for(i in 1:6){
    s = paste0("beta[",i,",",j,"]")
    beta_cors = rbind(beta_cors, high_cors[s == high_cors$col.param,])
  }
  for(i in 1:9){
    s = paste0("clan_effect[",i,",",j,"]")
    clan_cors = rbind(clan_cors, high_cors[s == high_cors$col.param,])
  }
  for(i in 1:4){
    s = paste0("ethnic_group_effect[",i,",",j,"]")
    ethnic_group_cors = rbind(ethnic_group_cors, high_cors[s == high_cors$col.param,])    
  }
  for(i in 1:5){
    s = paste0("marital_status_effect[",i,",",j,"]")
    marital_status_cors = rbind(marital_status_cors, high_cors[s == high_cors$col.param,])  
  }
  for(i in 1:5){
    sd_cors = rbind(sd_cors, high_cors[sd_params[i] == high_cors$col.param,])
  }
  for(i in 1:33){
    s = paste0("subclan_effect[",i,",",j,"]")
    subclan_cors = rbind(subclan_cors, high_cors[s == high_cors$col.param,])  
  }
  for(i in 1:4){
    s = paste0("territorial_section_effect[",i,",",j,"]")
    territorial_section_cors = rbind(territorial_section_cors, high_cors[s == high_cors$col.param,])  
  }
}





