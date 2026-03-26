nmbSimulations = 1000
n = 1000
m = 10
a = 2
b = 2

bruteForce_3c = function(nmbSimulations, n, m, a, b) {
  
  # Need to keep mean and mean^2 for calculating variance
  means = rep(0, nmbSimulations)
  means_squared = rep(0, nmbSimulations)
  
  for (i in 1:nmbSimulations) {
    
    # Simulate n values of Y from Beta(a,b)
    y = rbeta(n, shape1 = a, shape2 = b);
    
    # For each Y value, simulate X|Y ~ Binomial(m, y)
    x = rbinom(n, size = m, prob = y);
    
    means[i] = mean(x)
    means_squared[i] = mean(x^2)
  }
  
  variance = mean(means_squared) - (mean(means))^2
  
  print(paste("Empirical results with", 
              "nmbSimulations =", nmbSimulations,
              ", n =", n,
              ", m =", m,
              ", a =", a,
              ", b =", b
  ))
  print(paste("mean =", mean(means), "and variance =", variance))
}

rao_blackwell_3c = function(nmbSimulations, n, m, a, b) {
  
  # Need to keep mean and mean^2 for calculating variance
  means = rep(0, nmbSimulations)
  means_squared = rep(0, nmbSimulations)
  
  for (i in 1:nmbSimulations) {
    
    # Simulate n values of Y from Beta(a,b)
    y = rbeta(n, shape1 = a, shape2 = b);    
    
    # Calculate conditional expectation E(X|Y)
    # Since X|Y ~ Binomial(m, y), we have E(X|Y) = m * y
    condExpectation = m * y;
    
    means[i] = mean(condExpectation)
    means_squared[i] = mean(condExpectation^2)
  }

  variance = mean(means_squared) - (mean(means))^2
  
  print(paste("Rao-Blackwellized results with", 
              "nmbSimulations =", nmbSimulations,
              ", n =", n,
              ", m =", m,
              ", a =", a,
              ", b =", b
  ))
  print(paste("mean =", mean(means), "and variance =", variance))
}

bruteForce_3c(nmbSimulations, n, m, a, b)
rao_blackwell_3c(nmbSimulations, n, m, a, b)
