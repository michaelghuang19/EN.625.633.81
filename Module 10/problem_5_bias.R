data = c(4, 8, 12, 16, 20)
B = 1000

bootstrap_means = rep(0, B)
for (i in 1:B) {
  bootstrap_means[i] = mean(sample(data, length(data), replace = TRUE))
}

real_mean = mean(data)
bootstrap_mean = mean(bootstrap_means)
bias = bootstrap_mean - real_mean

print(paste("real_mean:", real_mean))
print(paste("bootstrap_mean:", bootstrap_mean))
print(paste("bias:", bias))