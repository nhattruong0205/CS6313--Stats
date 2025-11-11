# Mini Project 3
x <- c(21.72, 14.65, 50.42, 28.78, 11.23)

n <- length(x)

# -------------------------------
# (a) Analytical MLE for θ
# -------------------------------
# Derivation: θ_hat = n / sum(log(x))
theta_hat <- n / sum(log(x))
cat("Analytical MLE for θ =", round(theta_hat, 4), "\n")


# -------------------------------
# (b) Verify by computing manually
# -------------------------------
sum_log_x <- sum(log(x))
theta_hat_manual <- n / sum_log_x
cat("Manual computation gives θ =", round(theta_hat_manual, 4), "\n")

# -------------------------------
# (c) Numerical maximization using optim()
# -------------------------------
# Log-likelihood function
loglik <- function(theta) {
  if (theta <= 0) return(-Inf)
  n * log(theta) - (theta + 1) * sum(log(x))
}

# Maximize log-likelihood
result <- optim(par = 1, fn = function(th) -loglik(th), method = "Brent", lower = 0.0001, upper = 10)

result$par

# -------------------------------
# (d) Standard error and 95% CI
# -------------------------------
# Fisher Information I(θ) = n / θ^2
# Var(θ_hat) ≈ θ_hat^2 / n
se_theta <- theta_hat / sqrt(n)
ci_lower <- theta_hat - 1.96 * se_theta
ci_upper <- theta_hat + 1.96 * se_theta

cat("Standard Error =", round(se_theta, 4), "\n")
cat("95% CI for θ: (", round(ci_lower, 4), ",", round(ci_upper, 4), ")\n")
