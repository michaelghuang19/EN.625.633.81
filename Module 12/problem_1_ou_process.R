T = 1
N = 1000
theta = 1
mu = 20
sigma = 10

ornstein_uhlenbrek = function(T, N, theta, mu, sigma) {
  
  # loop over time steps
  dT = T / N
  timeIncrements = seq(0, T, dT)
  W_vec = rep(0, N+1)
  Xt = rep(0, N+1)
  X0 = 1
  Xt[1] = X0
  EM_soln = rep(0, N+1)
  EM_soln[1] = X0
  stochastic_integral = 0
  
  for (j in 1:N) {
    dW = rnorm(1, mean = 0, sd = sqrt(dT))
    W_vec[j+1] = W_vec[j] + dW
    
    # EM approximation for OU process
    EM = EM_soln[j] + theta * (mu - EM_soln[j]) * dT + sigma * dW
    EM_soln[j+1] = EM
    
    # Calculate exact solution;
    # Taking the full stochastic integral
    s = timeIncrements[j]
    t = timeIncrements[j+1]
    stochastic_integral = stochastic_integral + sigma * exp(theta * s) * dW
    
    constant_part = X0 * exp(-theta * t) + mu * (1 - exp(-theta * t))
    
    Xt[j+1] = constant_part + stochastic_integral * exp(-theta * t)
  }
  
  # Plot results
  par(mfrow = c(2,1));
  plot(timeIncrements, Xt);
  plot(timeIncrements, EM_soln);
}

ornstein_uhlenbrek(T, N, theta, mu, sigma)