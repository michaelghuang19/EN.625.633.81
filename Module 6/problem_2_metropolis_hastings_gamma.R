met_hastings_gamma = function(n, prop_shape, prop_rate) {
  
  x = rep(0, n)
  j = 1
  acc = 0
  x[1] = 2
  
  while (j < n) {
    
    # Sample candidate value
    x_cand = rgamma(1, shape = prop_shape, rate = prop_rate)
    
    # Calculate acceptance probability
    acc_prob = acc_prob_calc_gamma(x_cand, x[j], prop_shape, prop_rate)

    # Decide whether to accept or not
    acc_or_not = rbinom(1, 1, acc_prob);
    
    # if accept or not
    if (acc_or_not == 1) {
      acc = acc + 1;
      j = j + 1;
      x[j] = x_cand
    }
    else {
      j = j + 1
      x[j] = x[j-1]
    }	
    
    if (j %% 10 == 0) {
      print(paste("mean after", j, "iterations:", mean(x[1:j])))      
    }
  }
  
  acc_probability = acc/n;
  print(paste("Acceptance probability:", acc_probability));
  print(paste("Estimated mean:", mean(mean(x[1:n]))))
  hist(x, breaks = 50, density = 50, main = paste(
    "2. Metropolis-Hastings for Gamma(", prop_shape, ",", prop_rate, ")")
    );
}

acc_prob_calc_gamma = function(candidate, former, prop_shape, prop_rate) {
  
  if (candidate < 0) {
    acc_probability = 0;
  }
  else {
    # Target densities
    dens_candidate = dgamma(candidate, shape = 4.3, rate = 6.2)
    dens_former = dgamma(former, shape = 4.3, rate = 6.2)
    
    # Proposal densities
    g_former_given_candidate = dgamma(former, shape = prop_shape, rate = prop_rate)
    g_candidate_given_former = dgamma(candidate, shape = prop_shape, rate = prop_rate)
    
    acc_probability = (dens_candidate * g_former_given_candidate) / (dens_former * g_candidate_given_former);
  }
  
  acc_probability = min(1, acc_probability);
  return(acc_probability);
}

# 2. (b): Using Gamma(4, 7)
met_hastings_gamma(100, 4, 7)

# 2. (c): Using Gamma(5, 6)
met_hastings_gamma(100, 5, 6)
