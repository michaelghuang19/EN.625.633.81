n = 1000
nmbSimulations = 1000

antithetic_comparison = function(n, nmbSimulations) {
  brute_force_means = rep(0, nmbSimulations)
  antithetic_means = rep(0, nmbSimulations)
    
  for (i in 1:nmbSimulations) {
    # Brute force, simulate using inverse-transform directly
    U = runif(2 * n)
    X = 1 / sqrt(1 - U)
    brute_force_means[i] = mean(X)
    
    # Antithetic approach in pairs
    U = runif(n)
    X = 1 / sqrt(1 - U)
    Y = 1 / sqrt(U)
    antithetic_means[i] = mean((X + Y) / 2)
  }
      
  print(paste("Brute force mean estimate mean:", mean(brute_force_means)))
  print(paste("Brute force mean estimate variance:", var(brute_force_means)))
  print(paste("Antithetic mean estimate mean:", mean(antithetic_means)))
  print(paste("Antithetic mean estimate variance:", var(antithetic_means)))
  
  par(mfrow = c(1, 2))
  hist(brute_force_means, breaks = 60, main = "Brute Force Means")
  hist(antithetic_means, breaks = 60, main = "Antithetic Means")
}

antithetic_comparison(n, nmbSimulations)