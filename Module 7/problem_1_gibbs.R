# 1. (a)
n = 500
p = 5
rho = 0.25

gibbs_sample = function(n, p, rho) {
  # Precomputed constants for conditional distribution from Example 7.4
  conditional_mean_coeff = ((p - 1) * rho) / (1 + ((p - 2) * rho))
  sigma_squared = (1 + ((p - 2) * rho) - ((p - 1) * rho^2)) / (1 + ((p - 2) * rho))
  sigma = sqrt(sigma_squared)
  
  X <- array(0, dim = c(n, p)) # init arrays
  X[1, ] <- rnorm(p) # init chains
  
  for (t in 2:n) {
    x = X[t - 1, ]
    for (i in 1:p) {
      x_mean_minus = (sum(x) - x[i]) / (p - 1)
      x[i] = rnorm(1, mean = conditional_mean_coeff * x_mean_minus, sd = sigma)
    }
    X[t, ] = x
  }
  
  X
}

X = gibbs_sample(n, p, rho)
hist(X[, p], breaks = 50, probability = TRUE,
     main = paste("Marginals of X with p =", p, ", n =", n))
curve(dnorm(x, 0, 1), add = TRUE, col = "red")

# 1. (b)

library(mnormt)

norm_sample = function(n, p, rho) {
  # sigma as given in Exercise 7.5
  I = diag(p)
  J = matrix(1, p, p)
  sigma = ((1 - rho) * I + rho * J)
  
  rmnorm(n, mean = rep(0, p), varcov = sigma)
}

system.time(gibbs_sample(n, p, rho))
system.time(norm_sample(n, p, rho))
