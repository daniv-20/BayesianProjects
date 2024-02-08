library(tidyverse)



## bayes simulation for beta prior, beta posterior. 
## N_total is the total number of participants in the actual trial
## range is the list of numbers of successes we are using to update our posterior
bayes_sim = function(alpha.prior, beta.prior, N_total, range = c(0:20)){
simdat = data.frame(y = range, alpha.post = c(NA), beta.post = c(NA))

alpha.pri = c(rep(alpha.prior, length(range)))
beta.pri = c(rep(beta.prior, length(range)))
N = c(rep(N_total, length(range)))

## note: alpha new = alpha old + y new, beta new = beta old + N - y new

simdat$alpha.post = alpha.pri + simdat$y
simdat$beta.post = beta.pri + N - simdat$y

simdat = simdat %>%
  mutate(mean.post = round(alpha.post / (alpha.post + beta.post), digits = 2)) %>%
  mutate(var.post = round((alpha.post * beta.post) / ((alpha.post + beta.post)^2 * (alpha.post + beta.post + 1)), digits = 4))

return(simdat)
}


get_beta_post = function(alpha.prior, beta.prior, y, N){
  simdat = data.frame(y = y, alpha.post = c(NA), beta.post = c(NA))
  
  ## note: alpha new = alpha old + y new, beta new = beta old + N - y new
  
  simdat$alpha.post = alpha.prior + simdat$y
  simdat$beta.post = beta.prior + N - simdat$y
  
  simdat = simdat %>%
    mutate(mean.post = round(alpha.post / (alpha.post + beta.post), digits = 2)) %>%
    mutate(var.post = round((alpha.post * beta.post) / ((alpha.post + beta.post)^2 * (alpha.post + beta.post + 1)), digits = 4))
  
  return(simdat)
}


