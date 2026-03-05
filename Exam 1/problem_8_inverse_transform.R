problem_8_inverse_transform = function(n) {
  
  X = rep(0, n)
  i = 0
  
  while (i < n) {
    
    # Generate uniform for selecting distribution
    U = runif(1)
    # Generate uniform for generating variable
    V = runif(1)
    
    # Generate X-values based on V, using inverted CDF
    if (U < 0.25) {
      X[i] = V
    } else if (U >= 0.25 & U < 0.75) {
      X[i] = V^(1/4)
    } else {
      X[i] = V^(1/5)
    }
    
    i = i + 1
  }
  
  hist(X, breaks = 50, density = 50, 
       main=paste(n, "Generated Values using Weighted Inverse-Transform"));
}

problem_8_inverse_transform(10000)