problem_2_importance_sampling <- function(Nsim) {
  f = function(x) { 3.852985 * exp(-x^2 / sqrt(x)) * (sin(x))^2 }
  g = function(x) { exp(-x) }
  
  x = rexp(Nsim)
  weight = f(x) / g(x)
  
  # E[X] = X * weight
  # For E[X^2] = X^2 * weight
  weighted_x = x * weight
  weighted_x2 = x^2 * weight
  
  # Importance sampler is just the mean of the weights
  E_X = mean(weighted_x)
  E_X2 = mean(weighted_x2)
  Var_X = E_X2 - E_X^2
  
  par(mfrow=c(2,1))
  
  # Plot 1: Convergence of E[X] estimate
  plot(cumsum(weighted_x)/1:Nsim,
       main=paste("E[X] estimate over time with n=", Nsim))
  abline(h=E_X, col="red")
  
  # Plot 2: Convergence of Var(X) estimate
  cumsum_EX = cumsum(weighted_x)/1:Nsim
  cumsum_EX2 = cumsum(weighted_x2)/1:Nsim
  cumsum_Var = cumsum_EX2 - (cumsum_EX^2)
  
  plot(cumsum_Var,
       main=paste("Var(X) estimate over time with n=", Nsim))
  abline(h=Var_X, col="red")
  
  par(mfrow=c(1,1))
  
  print(paste("Estimated E[X]:", E_X))
  print(paste("Estimated Var(X):", Var_X))
}

problem_2_importance_sampling(100)
problem_2_importance_sampling(10000)