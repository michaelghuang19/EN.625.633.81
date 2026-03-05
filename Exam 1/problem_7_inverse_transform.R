problem_7_inverse_transform = function(n) {
  
  X = rep(0, n)
  i = 0

  while (i < n) {
    
    # Generate uniform
    U = runif(1)
    
    # Generate X-values based on U, using inverted CDF
    if (U < 0.5) {
      X[i] = log(2 * U) / 2
    } else {
      X[i] = -log(2 * (1 - U)) / 2
    }
    
    i = i + 1
  }

  hist(X, breaks = 50, density = 50, 
       main=paste(n, "Generated Values using Inverse-Transform"));
}

problem_7_inverse_transform(10000)