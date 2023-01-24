## function for running the model ##
norm_model = function(c_i = 0:10, #contribution values
                      N = 1000, #population size
                      n_g = 50, #group size
                      mu = rep(0.01,length(c_i)), #mutation rates
                      s = 4, #norm strength
                      w_0 = 100, #baseline
                      m = 1.5, #multiplier
                      q_0 = rep(1/length(c_i), length(c_i)), #initial frequencies
                      t_max = 5000) #number of generations
{
  #number of groups
  n = N/n_g
  #mutation matrix
  M = matrix(0, nrow = length(c_i), ncol = length(c_i))
  for(i in 1:length(c_i)){
    if(i==1)
      M[i,c(i,i+1)] = c(1-mu[i]/2, mu[i+1]/2)
    else if (i==length(c_i))
      M[i,c(i-1,i)] = c(mu[i-1]/2, 1-mu[i]/2)
    else
      M[i,c(i-1,i,i+1)] = c(mu[i-1]/2, 1-mu[i], mu[i+1]/2)
  }
  
  #pre-allocate parameter vectors/matrices
  q_i_g = matrix(0, nrow = length(c_i), ncol = n)
  q_i = vector(mode = "numeric", length = length(c_i))
  c_avg_g = vector(mode = "numeric", length = n)
  penalty_i_g = matrix(0, nrow = length(c_i), ncol = n)
  v_i_g = matrix(0, nrow = length(c_i), ncol = n)
  v_bar_g = vector(mode = "numeric", length = n)
  w_i = vector(mode = "numeric", length = length(c_i))
  freq_i = matrix(0, nrow = t_max, ncol = length(c_i))
  c_avg_pop = vector(mode = "numeric", length = t_max)
  
  ## SIMULATION ##
  set.seed(9834)
  for(i in 1:t_max){
    #sample group distributions from a multinomial distribution
    if(i == 1)
      X = rmultinom(n, size = n_g, prob = q_0)
    else
      X = rmultinom(n, size = n_g, prob = freq_i[i-1,])
    #within-group allele frequencies
    q_i_g = apply(X, 2, function(x) x/n_g)
    #allele frequencies in the whole population
    q_i = rowSums(q_i_g)/n
    #within-group average contribution values
    c_avg_g = apply(q_i_g, 2, function(x) sum(x*c_i))
    #within-group penalties
    penalty_i_g = apply(as.matrix(c_avg_g), 1, function(x) pmax(0,(x-c_i))*s)
    #within-group individual payoffs
    v_i_g = apply(as.matrix(c_avg_g), 1, function(x) m*x-c_i)-penalty_i_g
    #within_group average payoffs
    v_bar_g = colSums(v_i_g)/length(c_i)
    #fitness of contribution values
    w_i = w_0+rowSums(q_i_g*(v_i_g/(abs(v_bar_g)[col(v_i_g)])))/rowSums(q_i_g)
    w_i[w_i=="NaN"] = 0
    #average fitness of the population
    w_bar = sum(q_i*w_i)
    #allele frequencies in the next generation (effect of selection and mutation on allele frequencies)
    freq_i[i,] = rowSums(M%*%diag(q_i)%*%diag(w_i/w_bar))
    #average contribution in the whole population
    c_avg_pop[i] = mean(c_avg_g)
  }
  return(list(freq_i = freq_i, c_avg_pop = c_avg_pop))
}

## TEST RUN ##
c_i = 0:10
N = 1000000
n_g = 50000
mu = rep(0.01,length(c_i))
s = seq(0,4,0.5)
w_0 = 10
m = 1.5
# q_0 = rep(1/length(c_i), length(c_i))
q_0 = c(0,1,0,0,0,0,0,0,0,0,0)
t_max = 10000

start = Sys.time()
freqs = vector(mode = "list", length = length(s))

for(i in 1:length(s)){
  freqs[[i]] = norm_model(c_i = c_i,
                   N = N,
                   n_g = n_g,
                   mu = mu,
                   s = s[i],
                   w_0 = w_0,
                   m = m,
                   q_0 = q_0,
                   t_max = t_max)
}
end = Sys.time()
end-start
