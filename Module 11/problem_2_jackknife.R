num_samples = 100
n = 20
B = 2500
theta = 1

bootstrap_variances_x = rep(0, num_samples)
jackknife_variances_x = rep(0, num_samples)

for (i in 1:num_samples) {
  
  x = rnorm(n, mean = theta, sd = 1)
  theta_hat = mean(x)
  
  bootstrap_estimates = rep(0, B)
  for (j in 1:B) {
    bootstrap_sample = sample(x, n, replace = TRUE)
    bootstrap_estimates[j] = mean(bootstrap_sample)
  }
  bootstrap_variances_x[i] = var(bootstrap_estimates)

  jackknife_estimates = rep(0, n)
  for (k in 1:n) {
    jackknife_estimates[k] = mean(x[-k])
  }
  jackknife_variances_x[i] = (
    (n - 1) / n) * 
    sum(
      (jackknife_estimates - mean(jackknife_estimates)
    )^2
  )
}

print(paste("Real variance of X:", 1/n))
print(paste("Bootstrap Mean for Variance of X:", mean(bootstrap_variances_x)))
print(paste("Bootstrap SD for Variance of X:", sd(bootstrap_variances_x)))
print(paste("Jackknife Mean for Variance of X:", mean(jackknife_variances_x)))
print(paste("Jackknife SD for Variance of X:", sd(jackknife_variances_x)))

bootstrap_variances_x2 = rep(0, num_samples)
jackknife_variances_x2 = rep(0, num_samples)

for (i in 1:num_samples) {
  
  x = rnorm(n, mean = theta, sd = 1)
  theta_hat = mean(x)^2
  
  bootstrap_estimates = rep(0, B)
  for (j in 1:B) {
    bootstrap_sample = sample(x, n, replace = TRUE)
    bootstrap_estimates[j] = mean(bootstrap_sample)^2
  }
  bootstrap_variances_x2[i] = var(bootstrap_estimates)
  
  jackknife_estimates = rep(0, n)
  for (k in 1:n) {
    jackknife_estimates[k] = mean(x[-k])^2
  }
  jackknife_variances_x2[i] = (
    (n - 1) / n) * 
    sum(
      (jackknife_estimates - mean(jackknife_estimates)
      )^2
    )
}
  
print(paste("Real variance of X^2:", ((4 * n) + 2) / n^2))
print(paste("Bootstrap Mean for Variance of X^2:", mean(bootstrap_variances_x2)))
print(paste("Bootstrap SD for Variance of X^2:", sd(bootstrap_variances_x2)))
print(paste("Jackknife Mean for Variance of X^2:", mean(jackknife_variances_x2)))
print(paste("Jackknife SD for Variance of X^2:", sd(jackknife_variances_x2)))
