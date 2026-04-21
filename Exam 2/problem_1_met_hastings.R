n = 10000

met_hastings = function(n, sd) {
  x = rep(0, n)
  acc = 0
  j = 1
  x[1] = 4
  
  while (j < n) {
    # Propose candidate from normal distribution
    x_cand = rnorm(1, mean = x[j], sd = sd)
    
    # Calculate acceptance probability
    acc_prob = 0
    if (x_cand > 2) {
      acc_prob = min(1, (x[j] / x_cand)^3)
    }
    
    # Decide whether to accept value 
    acc_or_not = rbinom(1, 1, acc_prob)
    
    if (acc_or_not == 1) {
      acc = acc + 1;
      j = j + 1;
      x[j] = x_cand
    }
    else {
      j = j + 1
      x[j] = x[j-1]
    }	
  }
  
  acc_probability = acc / n
  print(paste("Acceptance probability:", acc_probability))
  print(paste("Estimated mean:", mean(x)))
  hist(x, breaks = 75, probability = TRUE,
       main = paste("Metropolitan-Hastings Sample with sd =", sd))
}

met_hastings(n, 1)
met_hastings(n, 2)
met_hastings(n, 4)