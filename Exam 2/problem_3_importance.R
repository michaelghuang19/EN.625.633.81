n = 1000

# h(x) = f(x) / g(x)
h = function(x) {
  2 * x^2 * sin(pi * x)
}

importance_sampling = function(n) {
  # Generate samples from our chosen g(x)
  x = rexp(n, rate = 1 / 2)
  
  # Evaluate h(x) at the sample points
  h_x = h(x)
  
  integral_estimate = mean(h_x)
  
  plot(
    cumsum(h_x) / 1:n,
    type = "l",
    xlab = "Number of samples",
    ylab = "Integral estimate",
    main = paste(
      "Importance Sampling Estimate with n =", 
      n,
      ", mean =",
      integral_estimate
    )
  )
  print(paste("Integral estimate:", integral_estimate))
}

importance_sampling(n)