problem_3_importance_sampling <- function(Nsim) {
  f = function(x) { 3.852985 * exp(-x^2 * sqrt(x)) * (sin(x))^2 }
  g = function(x) { exp(-(x - 3)) }
  
  x = rexp(Nsim) + 3
  weight = f(x) / g(x)
  
  # Importance sampler is just the mean of the weights
  P_hat = mean(weight)
  
  plot(cumsum(weight)/1:Nsim, 
       main=paste("Importance sampling estimate over time with n=", Nsim))
  abline(h=P_hat, col="red")
  
  print(paste("Importance sampling estimate of P(X>3):", P_hat))
}

problem_3_importance_sampling(100)
problem_3_importance_sampling(10000)