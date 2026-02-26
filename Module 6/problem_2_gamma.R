x_seq <- seq(0.01, 20, length.out = 100)

# Calculate ratio of f(x) / g(x) at each point
ratios <- dgamma(x_seq, shape=4.3, rate=6.2) / dgamma(x_seq, shape=4, rate=7)
print(ratios)

# The ratios seem to continue to infinity, 
# and took a long time to run when taking this M, 
# since the acceptance rate becomes extremely low.
# Instead, I used G(4,6) with a heavier tail to better model

ratios <- dgamma(x_seq, shape=4.3, rate=6.2) / dgamma(x_seq, shape=4, rate=6)
print(ratios)
M <- max(ratios)
print(M)

accept_reject_part_a = function(n, M) {
  
  simulated_values = rep(0, n)
  simulated_successes = 0
  simulated_trials = 0
  
  # Target distribution: Gamma(4.3, 6.2)
  target_shape = 4.3
  target_rate = 6.2

  # Candidate distribution: Gamma(4, 6) -- not Gamma(4, 7)
  cand_shape = 4
  cand_rate = 6
  
  while (simulated_successes < n) {
    
    simulated_trials = simulated_trials + 1
    
    x_cand = rgamma(1, shape=cand_shape, rate=cand_rate)
    uniform = runif(1, 0, M * dgamma(x_cand, shape=cand_shape, rate=cand_rate))
    
    # Accept if uniform < f(x_cand) where f is Gamma(4.3, 6.2)
    if (uniform < dgamma(x_cand, shape=target_shape, rate=target_rate)) {
      simulated_successes = simulated_successes + 1
      simulated_values[simulated_successes] = x_cand
    }
    
    if (simulated_trials %% 10 == 0) {
      print(mean(simulated_values))      
    }
  }
  
  accept_prob = simulated_successes / simulated_trials
  print(paste("Acceptance probability: ", accept_prob))
  print(paste("Estimated mean: ", mean(simulated_values)))
  hist(simulated_values, density = 50, breaks = 50, main = "2. (a): Accept-Reject")
}

accept_reject_part_a(100, M)