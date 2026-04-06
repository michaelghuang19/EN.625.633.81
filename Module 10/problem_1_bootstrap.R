data = c(1, 2, 3.5, 4, 7.3, 8.6, 12.4, 13.8, 18.1)

trimmed_mean = function(data) {
  sorted = sort(data)
  trimmed = sorted[3:(length(sorted) - 2)]
  return(mean(trimmed))
}

bootstrap_var = function(data, B) {
  trimmed_means = rep(0, B)
  for (i in 1:B) {
    x_star = sample(data, length(data), replace = TRUE)
    trimmed_means[i] = trimmed_mean(x_star)
  }
  return(var(trimmed_means))
}

B_values = c(25, 100, 200, 500, 1000, 2000)
for (B in B_values) {
  print(paste("B =", B))
  print(bootstrap_var(data, B))
}

var_matrix = matrix(0, 10, length(B_values))
colnames(var_matrix) = paste("B =", B_values)
rownames(var_matrix) = paste("Seed:", 1:10)

for (seed in 1:10) {
  set.seed(seed)
  for (i in seq_along(B_values)) {
    var_matrix[seed, i] = bootstrap_var(data, B_values[i])
  }
}

print(var_matrix)