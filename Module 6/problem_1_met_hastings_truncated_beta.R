met_hastings_truncated_beta = function(n, c, d, prop_function = "beta") {
  
  x = rep(0, n)
  j = 1
  acc = 0
  # Start in middle of truncation interval
  x[1] = (c + d)/2
  
  # We use Beta(2,6)
  prop_alpha = 2
  prop_beta = 6
  
  while (j < n) {
    
    # Sample candidate value, based on proposal function
    if (prop_function == "beta") {
      x_cand = rbeta(1, prop_alpha, prop_beta)
    } else if (prop_function == "uniform") {
      x_cand = runif(1, c, d)
    }
    
    # Calculate acceptance probability
    acc_prob = acc_prob_calc_truncated(x_cand, x[j], c, d, 
                                       prop_function, prop_alpha, prop_beta)
    
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
  print(paste("Interval: (", c, ",", d, ")"))
  print(paste("Proposal function:", prop_function))
  print(paste("Acceptance probability:", acc_probability))
  
  hist(x, breaks = 50, density = 50,
       main = paste("Truncated Beta(2.7, 6.3) on (", c, ",", d,  ") -", prop_function))
}


acc_prob_calc_truncated = function(candidate, former, c, d, 
                                   prop_function, prop_alpha, prop_beta) {
  
  # Reject if candidate is outside truncated interval
  if (candidate <= c || candidate >= d) {
    return(0)
  }
  
  # Truncated target densities, adjusted by integral
  dens_candidate = (dbeta(candidate, 2.7, 6.3) / 
    (pbeta(d, 2.7, 6.3) - pbeta(c, 2.7, 6.3)))
  dens_former = (dbeta(former, 2.7, 6.3) / 
    (pbeta(d, 2.7, 6.3) - pbeta(c, 2.7, 6.3)))
  
  # Proposal densities
  if (prop_function == "beta") {
    g_former_given_candidate = dbeta(former, prop_alpha, prop_beta)
    g_candidate_given_former = dbeta(candidate, prop_alpha, prop_beta)
  } else if (prop_function == "uniform") {
    # We can simplify for uniform, as it is symmetric: g(x|y) = g(y|x)
    g_former_given_candidate = 1 / (d - c)
    g_candidate_given_former = 1 / (d - c)
  }
  
  acc_probability = ((dens_candidate * g_former_given_candidate) / 
    (dens_former * g_candidate_given_former));
  
  acc_probability = min(1, acc_probability)
  return(acc_probability)
}

# 1. (b)

# Using c = 0.1, d = 0.9
met_hastings_truncated_beta(100, 0.1, 0.9, "beta")
met_hastings_truncated_beta(100, 0.1, 0.9, "uniform")

# Using c = 0.25, d = 0.75
met_hastings_truncated_beta(100, 0.25, 0.75, "beta")
met_hastings_truncated_beta(100, 0.25, 0.75, "uniform")


