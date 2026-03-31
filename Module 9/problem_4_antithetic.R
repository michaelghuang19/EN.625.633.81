nmbSimulations = 1000
n = 1000

generate_antithetic_comparison = function(nmbSimulations, n, inverse_cdf) {
  means_X = rep(0, nmbSimulations)
  medians_X = rep(0, nmbSimulations)
  means_XY = rep(0, nmbSimulations)
  medians_XY = rep(0, nmbSimulations)
  
  for (i in 1:nmbSimulations) {
    U = runif(n, 0, 1)
    X = inverse_cdf(U)
    Y = inverse_cdf(1 - U)
    XY = (X + Y) / 2
    
    # Using X only
    means_X[i] = mean(X)
    medians_X[i] = median(X)
      
    # Using both X and Y
    means_XY[i] = mean(XY)
    medians_XY[i] = median(XY)
  }
  
  print(paste("Results for", deparse(substitute(inverse_cdf)), ":"))
  print("Means:")
  print(paste("Average of means with X only:", mean(means_X)))
  print(paste("Variance of means with X only:", var(means_X)))
  print(paste("Average of means with X and Y:", mean(means_XY)))
  print(paste("Variance of means with X and Y:", var(means_XY)))
  print("Medians:")
  print(paste("Average of medians with X only:", mean(medians_X)))
  print(paste("Variance of medians with X only:", var(medians_X)))
  print(paste("Average of medians with X and Y:", mean(medians_XY)))
  print(paste("Variance of medians with X and Y:", var(medians_XY)))
}

inverse_cdf_f1 <- function(u) {
  tan(pi * (u - 0.5))
}

generate_antithetic_comparison(nmbSimulations, n, inverse_cdf_f1)

inverse_cdf_f2 <- function(u) {
  ifelse(u < 0.5, log(2 * u), -log(2 - (2 * u)))
}

generate_antithetic_comparison(nmbSimulations, n, inverse_cdf_f2)

inverse_cdf_f4 <- function(u) {
  qt(u, df = 3)
}

generate_antithetic_comparison(nmbSimulations, n, inverse_cdf_f4)


