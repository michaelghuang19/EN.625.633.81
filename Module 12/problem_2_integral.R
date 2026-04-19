T_values = c(0.5, 1, 5)
N = 1000
nmbSimulations = 1000

ito_strat_comparison = function(T, N, nmbSimulations) {
  
  ito_integrals = rep(0, nmbSimulations)
  strat_integrals = rep(0, nmbSimulations)
  W_final_squared = rep(0, nmbSimulations)
  
  for (iSim in 1:nmbSimulations) {
    dT = T / N
    timeIncrements = seq(0, T, dT)
    
    # Initialize Brownian motion
    W_values = rep(0, N+1)
    W_values[1] = 0
    
    # Generate Brownian motion path
    for (j in 1:N) {
      dW = rnorm(1, 0, sqrt(dT))
      W_values[j+1] = W_values[j] + dW
    }
    
    ito_integral = 0
    strat_integral = 0
    for (j in 1:N) {
      # Ito integral (just left endpoint)
      dW = W_values[j+1] - W_values[j]
      ito_integral = ito_integral + W_values[j] * dW
      
      # Stratonovich integral (calculate midpoint with delta Z adjustment)
      delta_z = rnorm(1, mean = 0, sd = sqrt(dT / 4))
      W_midpoint = 0.5 * (W_values[j] + W_values[j+1]) + delta_z
      strat_integral = strat_integral + W_midpoint * dW
    }
    
    ito_integrals[iSim] = ito_integral
    strat_integrals[iSim] = strat_integral
    W_final_squared[iSim] = W_values[N+1]^2
  }
  
  # Compute averages to compare values
  ito_int_avg = mean(ito_integrals)
  strat_int_avg = mean(strat_integrals)
  avg_W_squared = mean(W_final_squared)
  
  # Calculate theoretical values to compare
  ito_int_theoretical = 0.5 * avg_W_squared - 0.5 * T
  strat_int_theoretical = 0.5 * avg_W_squared
  
  sprintf("Results for T = %f, N = %d...", T, N)
  
  print("Ito Integral Results")
  print(paste("Simulated average:", ito_int_avg))
  print(paste("Theoretical value:", ito_int_theoretical))
  print(paste("Simulated - theoretical difference:", ito_int_avg - ito_int_theoretical))
  
  print("Ito Integral Results")
  print(paste("Simulated average:", strat_int_avg))
  print(paste("Theoretical value:", strat_int_theoretical))
  print(paste("Simulated - theoretical difference:", strat_int_avg - strat_int_theoretical))

  print("Ito-Stratanovich Difference Results")
  print(paste("Simulated difference:", ito_int_avg - strat_int_avg))
  print(paste("Theoretical difference:", ito_int_theoretical - strat_int_theoretical))
}

for (T in T_values) {
  ito_strat_comparison(T, N, nmbSimulations)
}
