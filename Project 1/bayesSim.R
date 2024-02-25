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