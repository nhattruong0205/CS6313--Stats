### Problem 1: Satellite lifetime simulation

set.seed(123)   # For reproducibility

# PDF of T (for superimposing density)
f_T <- function(t) {
  ifelse(t >= 0, 0.2 * exp(-0.1 * t) - 0.2 * exp(-0.2 * t), 0)
}

# (a) Analytical P(T > 15)
# P(T > 15) = ∫ f_T(t) dt from 15 to ∞
prob_analytical <- integrate(f_T, lower = 15, upper = Inf)$value
prob_analytical

# (b)