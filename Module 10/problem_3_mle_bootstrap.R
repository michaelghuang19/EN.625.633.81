n = 100
B = 1000
theta = 10
data = runif(n, 0, theta)
theta_hat = max(data)

boot_max = rep(0, B)
for (i in 1:B) {
  boot_sample = sample(data, n, replace = TRUE)
  boot_max[i] = max(boot_sample)
}

print(paste("True theta:", theta))
print(paste("MLE theta:", theta_hat))
print(paste("Bootstrap max:", max(boot_max)))
print(paste("Boostrap max variance:", var(boot_max)))
print("Bootstrap max range:")
print(range(boot_max))