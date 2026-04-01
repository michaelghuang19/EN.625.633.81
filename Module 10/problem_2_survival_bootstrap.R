survival = c(25, 42, 45, 46, 51, 103, 124, 146, 340, 396, 412, 879, 1112)

B = 1000

log_survival = log(survival)
boot_means_log = rep(0, B)

for (i in 1:B) {
  sample_log_survival = sample(log_survival, length(log_survival), replace = TRUE)
  boot_means_log[i] = mean(sample_log_survival)
}

# Taking the 95% CI means 2.5% on either side
ci_log = quantile(boot_means_log, c(0.025, 0.975))
# Exponentiate after taking log
ci_original_scale = exp(ci_log)

print("95% CI for log/exponentiated bootstrap:")
print(ci_original_scale)

boot_means_original = rep(0, B)

for (i in 1:B) {
  sample_survival = sample(survival, length(survival), replace = TRUE)
  boot_means_original[i] = mean(sample_survival)
}

ci_raw = quantile(boot_means_original, c(0.025, 0.975))

print("95% CI for original scale bootstrap:")
print(ci_raw)