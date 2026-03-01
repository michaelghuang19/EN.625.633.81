met_hastings_beta = function(n, prop_alpha, prop_beta) {
  
  x = rep(0, n)
  j = 1
  acc = 0
  # Due to the nature of the beta distribution
  x[1] = 0.5
  
  while (j < n) {
    
    # Sample candidate value
    x_cand = rbeta(1, prop_alpha, prop_beta)
    
    # Calculate acceptance probability
    acc_prob = acc_prob_calc_beta(x_cand, x[j], prop_alpha, prop_beta)
    
    # Decide whether to accept or not
    acc_or_not = rbinom(1, 1, acc_prob)
    
    # if accept or not
    if (acc_or_not == 1) {
      acc = acc + 1
      j = j + 1
      x[j] = x_cand
    }
    else {
      j = j + 1
      x[j] = x[j-1]
    }	
  }
  
  acc_probability = acc/n
  print(paste("Acceptance probability:", acc_probability))
  hist(x, breaks = 50, density = 50, main = paste(
    "1. Metropolis-Hastings for Beta(", prop_alpha, ",", prop_beta, ")")
  );
}

acc_prob_calc_beta = function(candidate, former, prop_alpha, prop_beta) {
  
  if (candidate <= 0 || candidate >= 1) {
    acc_probability = 0
  }
  else {
    # Target densities
    dens_candidate = dbeta(candidate, 2.7, 6.3)
    dens_former = dbeta(former, 2.7, 6.3)
    
    # Proposal densities
    g_former_given_candidate = dbeta(former, prop_alpha, prop_beta)
    g_candidate_given_former = dbeta(candidate, prop_alpha, prop_beta)
    
    acc_probability = (dens_candidate * g_former_given_candidate) / (dens_former * g_candidate_given_former);
  }
  
  acc_probability = min(1, acc_probability);
  return(acc_probability)
}

# 1. (a)

# Using Beta(1, 1)
met_hastings_beta(100, 1, 1)

# Using Beta(2, 2)
met_hastings_beta(100, 2, 2)

# Using Beta(2, 4)
met_hastings_beta(100, 2, 4)

# Using Beta(2, 5)
met_hastings_beta(100, 2, 5)

# Using Beta(3, 6)
met_hastings_beta(100, 3, 6)
