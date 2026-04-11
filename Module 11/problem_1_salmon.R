B = 2500

R = c(68, 77, 299, 220, 142, 287, 276, 115, 64, 206, 222, 205, 233, 228,
      188, 132, 285, 188, 224, 121, 311, 166, 248, 161, 226, 67, 201, 267,
      121, 301, 244, 222, 195, 203, 210, 275, 286, 275, 304, 214)

S = c(56, 62, 445, 279, 138, 428, 319, 102, 51, 289, 351, 282, 310,
      266, 256, 144, 447, 186, 389, 113, 412, 176, 313, 162, 368, 54,
      214, 429, 115, 407, 265, 301, 234, 229, 270, 478, 419, 490, 430, 235)

R_inverse = 1 / R
S_inverse = 1 / S

salmon_regression <- lm(R_inverse ~ S_inverse)
# intercept
beta1_hat = coef(salmon_regression)[1]
# slope
beta2_hat = coef(salmon_regression)[2] 

S_stable <- (1 - beta2_hat) / beta1_hat

residual_bootstrap = rep(0, B)
case_bootstrap = rep(0, B)

original_residuals = residuals(salmon_regression)
original_fitted = fitted(salmon_regression)

for (j in 1:B) {
  n = length(R)
  sample_index = sample(1:n, n, replace = TRUE)
  
  # Bootstrap the residuals
  resampled_residuals = salmon_regression$residuals[sample_index]
  altered_response = resampled_residuals + predict(salmon_regression)
  
  new_regression = lm(altered_response ~ S_inverse)
  new_beta1 = new_regression$coefficients[1]
  new_beta2 = new_regression$coefficients[2]
  
  residual_bootstrap[j] = (1 - new_beta2) / new_beta1
  
  # Bootstrap the cases
  resampled_R = R[sample_index]
  resampled_S = S[sample_index]
  resampled_R_inverse = 1 / resampled_R
  resampled_S_inverse = 1 / resampled_S
  
  new_regression = lm(resampled_R_inverse ~ resampled_S_inverse)
  new_beta1 = new_regression$coefficients[1]
  new_beta2 = new_regression$coefficients[2]
  
  case_bootstrap[j] = (1 - new_beta2) / new_beta1
}

# 95% Confidence Interval
residual_ci = quantile(residual_bootstrap, c(0.025, 0.975))
case_ci = quantile(case_bootstrap,  c(0.025, 0.975))

# Standard Error / Estimated Variance
residual_se = sd(residual_bootstrap);
case_se = sd(case_bootstrap);

print("Residuals Results")
sprintf("95%% CI: [%f, %f]", residual_ci[1], residual_ci[2])
print(paste("Standard Error:", residual_se))

print("Case Results")
sprintf("95%% CI: [%f, %f]", case_ci[1], case_ci[2])
print(paste("Standard Error:", case_se))

# Histograms
par(mfrow = c(2,1))
hist(residual_bootstrap)
hist(case_bootstrap)

# Bias
residual_bias = mean(residual_bootstrap) - S_stable
case_bias = mean(case_bootstrap) - S_stable

print(paste("Original S Esimate:", S_stable))
print(paste("Residual bootstrap bias:", residual_bias))
print(paste("Case bootstrap bias:", case_bias))
