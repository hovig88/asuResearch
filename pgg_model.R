#PUBLIC GOODS GAME MODEL

#start recording time
start.time = Sys.time()

#go to current directory and run the R script containing all the functions
source("./pgg_functions.R")

# number of individuals
n = 100
# multiplier
m = 1.5
# baseline fitness for all individuals in first generation
b = 15
# contribution scale, 0 = no contribution, 9 = maximum possible amount of contribution
c = 0:9
# number of generations
gen = 1000
# mutation rate
mu = 0.01

c_mins = c(4.5)
norm_intensities = c(0.1)
simulation  = c(100)

for(c_min in c_mins){
  for(norm_intensity in norm_intensities){
    for(n_sims in simulation){
      end_result = as.data.frame(matrix(data = NA, nrow = n_sims, ncol = gen))
      for(sim in 1:n_sims){
        
        # initialize a list that will be populated with the inherited contributions of each individual in each generation
        inherited_contribution = vector(mode = "list", length = gen)
        # initialize a list that will be populated with the actual contributions (i.e. after mutation) of each individual in each generation
        actual_contribution = vector(mode = "list", length = gen)
        # initialize a list that will be populated with the probabilities (fitness) of the contributions in each generation
        fitness = vector(mode = "list", length = gen)

        # sample inherited contributions for the first generation (all inherited contributions will have equal weights in the first generation)
        inherited_contribution[[1]] = inherited_c(c, n)
        # some of the inherited contribution values will change due to mutation (social learning)
        # actual contributions for the first generation after mutants are introduced
        actual_contribution[[1]] = mutation(inherited_contribution[[1]], c, n, mu)
        # fitness of each individual in the first generation
        fitness[[1]] = f(actual_contribution[[1]], b, m, n, c_min, norm_intensity)
        
        for(i in 2:gen){
          inherited_contribution[[i]] = inherited_c(actual_contribution[[i-1]], n, fitness[[i-1]])
          actual_contribution[[i]] = mutation(inherited_contribution[[i]], c, n, mu)
          fitness[[i]] = f(actual_contribution[[i]], b, m, n, c_min, norm_intensity)
        }
        
        actual_contribution = unlist(lapply(actual_contribution, mean))
        end_result[sim,] = actual_contribution
      }
      
      end_result = colMeans(end_result)
      plot(1:gen, end_result, type = "l", lwd = 1.25,
           xlab = "Generation", ylab = "Contribution", main = paste("Norm intensity = ", 
                                                                    norm_intensity, " (", n_sims, " simulations)"))
    }
  }
}

#add main title
#mtext(expression(bold("Public goods game (with punishment)")), side = 3, line = 1, outer = TRUE, cex = 1.15, ps = 50)

#stop recording time
end.time = Sys.time()
time.taken = round(end.time - start.time,2)
time.taken