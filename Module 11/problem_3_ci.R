B_values = c(100, 500, 1000, 2500, 5000, 10000)
# Repeat multiple times to get to steady state
repetitions = 100

n = 50
mu_1 = 3
sigma_1 = 1

m = 50
mu_2 = 4
sigma_2 = 2

X = rnorm(n, mean = mu_1, sd = sigma_1)
Y = rnorm(m, mean = mu_2, sd = sigma_2)

# For calculated CI, use Welch's t-test which does not assume equal variances
normal_t_test = t.test(X, Y, conf.level = 0.95)
calculated_ci = normal_t_test$conf.int

bootstrap_ci = function(X, Y, B) {
  bootstrap_differences = rep(0, B)
  for (j in 1:B) {
    boot_x = sample(X, n, replace = TRUE)
    boot_y = sample(Y, m, replace = TRUE)
    
    bootstrap_differences[j] = mean(boot_x) - mean(boot_y)
  }
  
  bootstrap_ci = quantile(bootstrap_differences, probs = c(0.025, 0.975))
  
}

bootstrap_results = data.frame(B = integer(), ci_lower = numeric(), ci_upper = numeric())
for (B in B_values) {
  
  lower_results = rep(0, repetitions)
  upper_results = rep(0, repetitions)
  for (r in 1:repetitions) {
    bootstrap_result = bootstrap_ci(X, Y, B)
    lower_results[r] = bootstrap_result[1]
    upper_results[r] = bootstrap_result[2]
  }
  
  bootstrap_results = rbind(bootstrap_results, data.frame(
    B = B,
    lower_mean = mean(lower_results),
    lower_sd = sd(lower_results),
    upper_mean = mean(upper_results),
    upper_sd = sd(upper_results)
  ))
}

sprintf("Calculated CI: [%f, %f]", calculated_ci[1], calculated_ci[2])

print(bootstrap_results)

# Lower means, normal and log
par(mfrow = c(1, 2))
plot(bootstrap_results$B, bootstrap_results$lower_mean,
     type = "b", 
     ylim = range(c(bootstrap_results$lower_mean - bootstrap_results$lower_sd,
                    bootstrap_results$lower_mean + bootstrap_results$lower_sd)),
     xlab = "B", 
     ylab = "Mean of CI lower bound",
     main = "CI Lower Bound Mean over B")
arrows(bootstrap_results$B,
       bootstrap_results$lower_mean - bootstrap_results$lower_sd,
       bootstrap_results$B,
       bootstrap_results$lower_mean + bootstrap_results$lower_sd,
       code=0
)
abline(h = calculated_ci[1], col = "red")
plot(bootstrap_results$B, bootstrap_results$lower_mean,
     type = "b", 
     log = "x",
     ylim = range(c(bootstrap_results$lower_mean - bootstrap_results$lower_sd,
                    bootstrap_results$lower_mean + bootstrap_results$lower_sd)),
     xlab = "B (log scale)", 
     ylab = "Mean of CI lower bound",
     main = "CI Lower Bound Mean over B (log scale)")
arrows(bootstrap_results$B,
       bootstrap_results$lower_mean - bootstrap_results$lower_sd,
       bootstrap_results$B,
       bootstrap_results$lower_mean + bootstrap_results$lower_sd,
       code=0
)
abline(h = calculated_ci[1], col = "red")

# Lower SDs, normal and log
par(mfrow = c(1, 2))
plot(bootstrap_results$B, bootstrap_results$lower_sd,
     type = "b", 
     xlab = "B", 
     ylab = "SD of CI lower bound",
     main = "CI Lower Bound SD over B")
plot(bootstrap_results$B, bootstrap_results$lower_sd,
     type = "b", 
     log = "x",
     xlab = "B (log scale)", 
     ylab = "SD of CI lower bound",
     main = "CI Lower Bound SD over B (log scale)")

# Upper means, normal and log
par(mfrow = c(1, 2))
plot(bootstrap_results$B, bootstrap_results$upper_mean,
     type = "b", 
     ylim = range(c(bootstrap_results$upper_mean - bootstrap_results$upper_sd,
                    bootstrap_results$upper_mean + bootstrap_results$upper_sd)),
     xlab = "B", 
     ylab = "Mean of CI Upper Bound",
     main = "CI Upper Bound Mean over B")
arrows(bootstrap_results$B,
       bootstrap_results$upper_mean - bootstrap_results$upper_sd,
       bootstrap_results$B,
       bootstrap_results$upper_mean + bootstrap_results$upper_sd,
       code=0
)
abline(h = calculated_ci[2], col = "red")
plot(bootstrap_results$B, bootstrap_results$upper_mean,
     type = "b", 
     log = "x",
     ylim = range(c(bootstrap_results$upper_mean - bootstrap_results$upper_sd,
                    bootstrap_results$upper_mean + bootstrap_results$upper_sd)),
     xlab = "B (log scale)", 
     ylab = "Mean of CI Upper Bound",
     main = "CI Upper Bound Mean over B (log scale)")
arrows(bootstrap_results$B,
       bootstrap_results$upper_mean - bootstrap_results$upper_sd,
       bootstrap_results$B,
       bootstrap_results$upper_mean + bootstrap_results$upper_sd,
       code=0
)
abline(h = calculated_ci[2], col = "red")

# Upper SDs, normal and log
par(mfrow = c(1, 2))
plot(bootstrap_results$B, bootstrap_results$upper_sd,
     type = "b",
     xlab = "B", 
     ylab = "SD of CI Upper Bound",
     main = "CI Upper Bound SD over B")
plot(bootstrap_results$B, bootstrap_results$upper_sd,
     type = "b", 
     log = "x",
     xlab = "B (log scale)", 
     ylab = "SD of CI Upper Bound",
     main = "CI Upper Bound SD over B (log scale)")