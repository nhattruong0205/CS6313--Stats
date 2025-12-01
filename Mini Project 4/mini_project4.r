# Clear workspace
rm(list = ls())

# --- Load the Data ---
# Ensure VOLTAGE.csv is in your working directory
data <- read.csv("VOLTAGE.csv")

# Inspect the data structure
str(data)
head(data)

# Separate the data into two vectors for easier handling
# Location 0 = Remote, Location 1 = Local
remote <- data$voltage[data$location == 0]
local <- data$voltage[data$location == 1]

# Count of samples
n_remote <- length(remote)
n_local <- length(local)

# ============================================================
# Part (a): Exploratory Analysis
# ============================================================

# 1. Summary Statistics
cat("\n--- Summary Statistics: Remote (0) ---\n")
summary(remote)
cat("Standard Deviation:", sd(remote), "\n")

cat("\n--- Summary Statistics: Local (1) ---\n")
summary(local)
cat("Standard Deviation:", sd(local), "\n")

# 2. Visualizations
# Set up a 1x2 plotting area to show Boxplot and Histograms side-by-side
par(mfrow = c(1,1))   # reset to a single plot pane

# Boxplot comparing both locations
boxplot(remote, local,
    names = c("Remote (0)", "Local (1)"),
    main = "Boxplot of Voltage by Location",
    ylab = "Voltage",
    col = c("lightblue", "lightgreen")
)

# Histograms
hist(remote, main = "Histogram: Remote (0)", xlab = "Voltage", col = "lightblue")
hist(local, main = "Histogram: Local (1)", xlab = "Voltage", col = "lightgreen")

# ============================================================
# Part (b): Confidence Interval & Assumptions
# ============================================================

# 1. Assumption: Normality (Shapiro-Wilk Test)
# H0: Data is normally distributed
cat("\n--- Shapiro-Wilk Test for Normality ---\n")
shapiro_remote <- shapiro.test(remote)
shapiro_local <- shapiro.test(local)

print(shapiro_remote)
print(shapiro_local)

# 2. Assumption: Equality of Variances
# We can use var.test() (F-test) which is standard in base R, 
# or Levene's test (from 'car' package) if available. 
# Here we use var.test for standard base R compatibility.
# H0: Variances are equal (ratio = 1)
cat("\n--- F-test to compare two variances ---\n")
var_test_result <- var.test(remote, local)
print(var_test_result)

# Interpretation logic for the script user:
# If p-value of var.test > 0.05, we assume equal variances (var.equal = TRUE).
# If p-value < 0.05, we assume unequal variances (var.equal = FALSE).

cat("\n--- Two Sample t-test (95% Confidence Interval) ---\n")
t_test_result <- t.test(remote, local, var.equal = TRUE, conf.level = 0.95)

print(t_test_result)

# ============================================================
# Section 2: R Code for Question 2 (VAPOR.CSV Analysis)
# ============================================================

# 1. Load the Data
# Ensure VAPOR.csv is in your working directory
data_vapor <- read.csv("VAPOR.csv")

# Display column names and structure
cat("--- Data Structure ---\n")
str(data_vapor)

# 2. Calculate the Differences (d)
# We define the difference as: d = Experimental - Theoretical
data_vapor$Difference <- data_vapor$experimental - data_vapor$theoretical

# Extract the differences for analysis
differences <- data_vapor$Difference

# 3. Verification of Normality Assumption
# The paired t-test requires the differences to be normally distributed.
# H0: Differences are normally distributed
cat("\n--- Shapiro-Wilk Test for Normality of Differences ---\n")
shapiro_diff <- shapiro.test(differences)
print(shapiro_diff)

# Visualization for Normality (Q-Q Plot)
# A Q-Q plot helps visually assess normality. Points should fall close to the line.
cat("\n--- Q-Q Plot of Differences ---\n")
qqnorm(differences, main = "Q-Q Plot of Vapor Pressure Differences")
qqline(differences)


# 4. Perform Paired Samples t-test
# H0: mu_d = 0 (Mean difference is zero, model is good)
# Ha: mu_d != 0 (Mean difference is not zero, model is not good)
cat("\n--- Paired Samples t-test Results (H0: Mean Difference = 0) ---\n")
t_test_result <- t.test(
  data_vapor$experimental,
  data_vapor$theoretical,
  paired = TRUE,  # Crucial argument for paired t-test
  conf.level = 0.95
)

print(t_test_result)