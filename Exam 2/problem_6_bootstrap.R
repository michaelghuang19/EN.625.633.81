data = c(1.491, 0.019, 0.318, 0.056, 0.816, 0.978, 0.174, 0.461, 
         0.495, 0.547, 0.123, 2.270, 0.026, 0.041, 0.391)
n = length(data)
B = 1000
threshold = 0.75

bootstrap_prob = function(data, B, threshold) {
  # Clever way to calculate probability using one-hot encoding
  bootstrap_estimate = mean(data < threshold)
  
  bootstrap_estimates = rep(0, B)
  for (i in 1:B) {
    bootstrap_sample = sample(data, n, replace = TRUE)
    bootstrap_estimates[i] = mean(bootstrap_sample < threshold)
  }
  
  percentile_ci = quantile(bootstrap_estimates, c(0.025, 0.975))
  
  print(paste("Bootstrap estimate:", bootstrap_estimate))
  print(
    sprintf(
      "95%% CI for bootstrap estimate: (%f, %f)",
      percentile_ci[1], 
      percentile_ci[2]
    )
  )
  
  hist(
    bootstrap_estimates,
    main = paste(
      "Bootstrap distribution of Probability that RV <",
      threshold,
      "with B =",
      B
    )
  )
  
  abline(
    v = bootstrap_estimate,
    col = "red"
  )
}

bootstrap_prob(data, B, threshold)

# Follow up, using Unif(0, 1)
n = 15
B = 1000
threshold = 0.75
nmbSimulations = 1000

bootstrap_study = function(n, B, threshold, nmbSimulations) {
  probability_estimates = rep(0, nmbSimulations)
  
  for (i in 1:nmbSimulations) {
    unif_data = runif(n, 0, 1)
    
    bootstrap_estimates = rep(0, B)
    for (j in 1:B) {
      bootstrap_sample = sample(unif_data, n, replace = TRUE)
      bootstrap_estimates[j] = mean(bootstrap_sample < threshold)
    }
    
    probability_estimates[i] = mean(bootstrap_estimates)
  }
  
  true_probability = threshold / 1
  simulated_probability = mean(probability_estimates)
  
  print(paste("True probability:", true_probability))
  print(paste("Average bootstrap probability estimate:", simulated_probability))
  print(paste("Diff:", true_probability - simulated_probability))
  
  hist(
    probability_estimates,
    main = paste(
      "Bootstrap distribution of P( RV <",
      threshold,
      ") with B =",
      B,
      "for Unif(0,1)"
    )
  )
  
  abline(
    v = true_probability,
    col = "red"
  )
}

bootstrap_study(n, B, threshold, nmbSimulations)