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

# Function to simulate satellite_lifetime
simulate_satellite_lifetime <- function() {
  # Simulate lifetimes for blocks A and B
  # Because the mean is 10, rate = 1/10 = 0.1
  X_A <- rexp(1, rate = 0.1)  
  X_B <- rexp(1, rate = 0.1)  
  
  # Satellite lifetime is the maximum of the two blocks/ parallel
  T <- max(X_A, X_B)
  
  return(T)
}

# (i) Stimulate one draw of the block lifetimes XA and XB. 
simulate_satellite_lifetime()

# (b) Monte Carlo Simulation
# Function to run Monte Carlo simulation
run_monte_carlo <- function(n_reps) {
  # Generate n_reps draws of T
  T_draws <- replicate(n_reps, simulate_satellite_lifetime())
  
  # Estimate E(T)
  E_T_estimate <- mean(T_draws)
  
  # Estimate P(T > 15)
  prob_estimate <- mean(T_draws > 15)
  
  return(list(T_draws = T_draws, E_T = E_T_estimate, prob = prob_estimate))
}

#(ii) Repeated the previous step 1000 times
mc_1000 <- run_monte_carlo(1000)

#(iii) Create the histogram 
hist(mc_1000$T_draws, probability = TRUE, breaks = 30,
     main = "Histogram of Satellite Lifetime (1000 replications)",
     xlab = "Lifetime (years)", col = "lightblue", border = "white")

# Superimpose the theoretical density
curve(f_T(x), from = 0, to = max(mc_1000$T_draws), 
      add = TRUE, col = "red", lwd = 2)

legend("topright", legend = c("Simulated", "Theoretical"), 
       fill = c("lightblue", NA), border = c("black", NA),
       lty = c(NA, 1), col = c(NA, "red"), lwd = c(NA, 2))

# (iv): Estimate E(T)
cat("\nEstimate of E(T):", mc_1000$E_T, "\n")
cat("Exact E(T): 15\n")
# (v): Core E(T) with the one in a)
cat("Difference:", abs(mc_1000$E_T - 15), "\n\n")

#(vi)
# Repeated the run 4 more times
for (i in 1:4){
    # Core E(T) with the one in a)
    mc_1000 = run_monte_carlo(1000)
    
    cat("\nEstimate of E(T):", mc_1000$E_T, "\n")
    cat("Differences", abs(mc_1000$E_T - 15))
}

for (i in 1:4){
    # Core E(T) with the one in a)
    mc_100 = run_monte_carlo(100)
    cat("\nEstimate of E(T):", mc_100$E_T, "\n")
    cat("Differences", abs(mc_100$E_T - 15))
}

for (i in 1:4){
    # Core E(T) with the one in a)
    mc_10000 = run_monte_carlo(10000)
    cat("\nEstimate of E(T):", mc_10000$E_T, "\n")
    cat("Differences", abs(mc_10000$E_T - 15))
}

# Question 2: 10 points) Use a Monte Carlo approach to estimate the value of π based on 10,000 replications.
for (i in 1:5){
  n <- 10000

  # Generate 10000 points in unit square
  x <- runif(n,0,1)
  y <- runif(n,0,1)

  # Check if these points are inside the circle 
  inside <- (x - 0.5)^2 + (y - 0.5)^2 <= 0.25 # Circule formula 

  # Circle area = pi * r^2 = pi * (0.5)^2 = 0.25 * pi => pi = circleare * 4
  pi_estimate[i] <- 4 * mean(inside)
  cat("Run", i, ", estimate pi is ", pi_estimate[i], "\n")
}

cat("Average estimate pi: ", mean(pi_estimate), "\n")