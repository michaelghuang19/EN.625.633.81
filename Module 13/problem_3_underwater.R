data = readRDS("underwaterVehicle.rds")
Y = as.matrix(data)
n = nrow(Y)
dimensions = 6


F = rbind(
  c(1, 0, 0, 1, 0, 0),
  c(0, 1, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 1),
  c(0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 0, 1)
)

G = diag(dimensions)
Q = 0.02 * diag(dimensions)
R = 0.01 * diag(dimensions)

X_hat_pred = matrix(0, nrow = n, ncol = dimensions)
X_hat_filt = matrix(0, nrow = n, ncol = dimensions)
# Choose to use lists instead here as the syntax is nicer than 3D array
Omega = list()
Delta = list()
Theta = list()

# Initial predictors of 0, and 6 scalar matrix for error 
X_hat_pred[1, ] = rep(0, dimensions)
Omega[[1]] = dimensions * diag(dimensions)
Delta[[1]] = G %*% Omega[[1]] %*% t(G) + R
Theta[[1]] = F %*% Omega[[1]] %*% t(G)

for (j in 2:n) {
  
  # Prediction
  
  X_hat_pred[j, ] = F %*% X_hat_pred[j-1, ] +
    Theta[[j-1]] %*% solve(Delta[[j-1]]) %*%
    (Y[j-1, ] - G %*% X_hat_pred[j-1, ])
  
  Omega[[j]] = F %*% Omega[[j-1]] %*% t(F) + Q - 
    Theta[[j-1]] %*% solve(Delta[[j-1]]) %*% t(Theta[[j-1]])
  
  Delta[[j]] = G %*% Omega[[j]] %*% t(G) + R
  Theta[[j]] = F %*% Omega[[j]] %*% t(G)

  # Filtering 
  X_hat_filt[j, ] = X_hat_pred[j, ] +
    Omega[[j]] %*% t(G) %*% solve(Delta[[j]]) %*%
    (Y[j, ] - G %*% X_hat_pred[j, ])
}

print("Estimated position at time 53")
print(paste("East position (u):", X_hat_filt[53, 1]))
print(paste("North position (y):", X_hat_filt[53, 2]))
print(paste("Down position (z):", X_hat_filt[53, 3]))

print("Raw position at time 53")
print(paste("East position (u):", Y[53, 1]))
print(paste("North position (y):", Y[53, 2]))
print(paste("Down position (z):", Y[53, 3]))