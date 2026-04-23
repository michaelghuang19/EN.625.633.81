Kalman_Filtering_Example_With_Smoothing = function(target, n) {
  
  #####  Establishing Sigma Values
  sigma_V = .2;
  sigma_W = .1;
  
  ##### Creating storage vectors for X, Y, X_pred, X_filt, Omega, Delta and Theta
  X = rep(0, n);
  Y = rep(0, n);
  X_hat_pred = rep(0,n);
  X_hat_filt = rep(0,n);
  Omega = rep(0,n);
  Delta = rep(0,n);
  Theta = rep(0,n);
  
  ###### Starting everything with t = 1 
  X[1] = rnorm(1, 0, sigma_V);
  Y[1] = X[1] + rnorm(1,0, sigma_W);
  X_hat_pred[1] = .2;
  Omega[1] = .04;
  Delta[1] = Omega[1] + sigma_W^2;
  Theta[1] = Omega[1]
  
  for (j in 2:n) {
    
    #########  doing prediction ##########
    X_hat_pred[j] = X_hat_pred[j-1] + (Theta[j-1]/Delta[j-1])*(Y[j-1] - X_hat_pred[j-1]);
    Omega[j] = Omega[j-1] + sigma_V^2 - Theta[j-1]^2/Delta[j-1];
    Delta[j] = Omega[j] + sigma_W^2;
    Theta[j] = Omega[j];
    
    ######## simulating next value of X and Y ###########
    X[j] = X[j-1] + rnorm(1, 0, sigma_V);
    Y[j] = X[j] + rnorm(1, 0, sigma_W);
    
    ####### filtering   ############
    X_hat_filt[j] = X_hat_pred[j] + (Omega[j]/Delta[j])*(Y[j] - X_hat_pred[j]);
  }
  
  # [NEW] Smoothing
  
  Omega_filt_target = Omega[target] - Omega[target]^2 / Delta[target]
  X_hat_smooth = X_hat_filt[50]
  Omega_cross = Omega_filt_target
  Omega_smooth = Omega_filt_target
  
  for (k in (target + 1):n) {
    Omega_cross = Omega_cross * (1 - (Theta[k-1] / Delta[k-1]))
    X_hat_smooth = X_hat_smooth + (Omega_cross / Delta[k]) * (Y[k] - X_hat_pred[k])
    Omega_smooth = Omega_smooth - (Omega_cross^2 / Delta[k])
  }
  
  
  
  plot(X, type = "l")
  lines(X_hat_pred, type = "l", col = "red");
  lines(X_hat_filt, type = "l", col = "blue");
  legend(x = "topleft", legend=c("True X", "Predicted X", "Filtered X"),
         col=c("black", "red", "blue"), lty = c(1,1,1))
  
  print(paste("Results with target =", target))
  print(paste("True X[target]:", X[target]))
  print(paste("X_hat_pred[target]:", X_hat_pred[target]));
  print(paste("X_hat_filt[target]:", X_hat_filt[target]));
  print(paste("X_hat_smooth at target =", target, ":", X_hat_smooth))
  print(paste("Prediction error cov:", Omega[target]))
  print(paste("Filtered error cov:", Omega_filt_target))
  pr
  cat(":  ", , "\n")
  cat("Filtered error cov:    ", Omega_filt_target, "\n")
  cat("Smoothed error cov:    ", Omega_smooth, "\n")
}

Kalman_Filtering_Example_With_Smoothing(50, 100)