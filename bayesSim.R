<<<<<<< refs/remotes/origin/main
library(tidyverse)

bayes_sim = function(alpha.prior = 1, beta.prior = 1, N_total = 20, events = 0:20, theta_target = 0.25){
  ## bayes simulation for beta prior, beta posterior. 
  
  #### Inputs
  ## N_total - the total number of participants in the actual trial
  ## events - the list of numbers of successes we are using to update our posterior
  ## theta_target - the target effect size 
  
  #### Outputs
  ## simdat - data frame containing:
  ### events, N_total, alpha.prior, beta.prior - input parameters
  ### alpha.post, beta.post - posterior distribution parameters
  ### Pr[theta <= theta_target] - probability that the true effect size 
  ###                             is LESS THAN the target effect size
  
simdat = data.frame(events = events, 
                    N_total = N_total,
                    alpha.prior = alpha.prior, 
                    beta.prior = beta.prior) %>%
  
  ## note: alpha new = alpha old + y new, beta new = beta old + N - y new
  mutate(alpha.post = alpha.prior + events,
         beta.post = beta.prior + N_total - events,
         `Pr[theta <= theta_target]` = pbeta(theta_target, alpha.post, beta.post))
return(simdat)
}
=======
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

simdat %>% flextable::flextable()
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
  
  simdat %>% flextable::flextable()
  return(simdat)
}

## Generate patients and outcomes for binary outcome
gen_binom_pats = function(N_pats, prob_success){
  results = list()
  results$pats = data.frame(sid = c(1:N_pats), outcome = rbinom(N_pats, 1, prob_success))
  results$summary = data.frame(y = sum(results$pats$outcome), N = N_pats)
  return(results)
  }

>>>>>>> Added function to generate N patients with binary outcome of given success probability
