nmbSimulations = 1000
n = 1000
a = 5
b = 1

bruteForce_3b = function(nmbSimulations, n, a, b) {
  
  # Need to keep mean and mean^2 for calculating variance
  means = rep(0, nmbSimulations)
  means_squared = rep(0, nmbSimulations)
  
  for (i in 1:nmbSimulations) {
    
    # Simulate n values of Y from Gamma(a,b)
    y = rgamma(n, shape = a, scale = b)
    
    # For each Y value, simulate X|Y ~ N(0, y)
    x = rnorm(n, mean = 0, sd = sqrt(y))
    
    means[i] = mean(x)
    means_squared[i] = mean(x^2)
  }
  
  variance = mean(means_squared) - (mean(means))^2
  
  print(paste("Empirical results with", 
              "nmbSimulations =", nmbSimulations,
              ", n =", n,
              ", a =", a,
              ", b =", b
              ))
  print(paste("mean =", mean(means), "and variance =", variance))
}

rao_blackwell_3b = function(nmbSimulations, n, a, b) {
  
  # Need to keep mean and mean^2 for calculating variance
  means = rep(0, nmbSimulations)
  means_squared = rep(0, nmbSimulations)
  
  for (i in 1:nmbSimulations) {
    
    # Simulate n values of Y from Gamma(a,b)
    y = rgamma(n, shape = a, scale = b)
    
    # Calculate conditional expectation E(X|Y)
    # Since X|Y ~ N(0, y), we have E(X|Y) = 0
    condExpectation = rep(0, n)
    
    means[i] = mean(condExpectation)
    means_squared[i] = mean(condExpectation^2)
  }
  
  variance = mean(means_squared) - (mean(means))^2
  
  print(paste("Rao-Blackwellized results with", 
              "nmbSimulations =", nmbSimulations,
              ", n =", n,
              ", a =", a,
              ", b =", b
  ))
  print(paste("mean =", mean(means), "and variance =", variance))
}

bruteForce_3b(nmbSimulations, n, a, b)
rao_blackwell_3b(nmbSimulations, n, a, b)
