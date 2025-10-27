# Reset variables
rm(list = ls())

# Load required libraries
library(ggplot2)

# Read the dataset
roadrace <- read.csv("roadrace.csv")

# ============================================================
# QUESTION 1(a): Bar graph of Maine variable
# ============================================================

# Create frequency table for Maine variable
maine_table <- table(roadrace$Maine)
print("Frequency table for Maine variable:")
print(maine_table)

# Calculate proportions
maine_prop <- prop.table(maine_table)
print("Proportions:")
print(maine_prop)

png("barplot_maine.png", width = 800, height = 600) # Open file for saving

# Create bar graph
bp <- barplot(maine_table,
    main = "Distribution of Runners by Location",
    xlab = "Runner Location",
    ylab = "Frequency",
    col = c("lightblue", "lightcoral"),
    ylim = c(0, max(maine_table) * 1.15)
)

# Add count labels on top of bars
text(
    x = bp,
    y = maine_table,
    labels = maine_table,
    pos = 3
)

dev.off() # Close and save the file

# ============================================================
# QUESTION 1(b): Histograms of runners' times by location
# ============================================================

# Subset data by Maine status
maine_runners <- roadrace[roadrace$Maine == "Maine", ]
away_runners <- roadrace[roadrace$Maine == "Away", ]

# Summary statistics for Maine runners
cat("\n=== Summary Statistics for Maine Runners (Time in minutes) ===\n")
cat("Mean:", mean(maine_runners$Time..minutes., na.rm = TRUE), "\n")
cat("Standard Deviation:", sd(maine_runners$Time..minutes., na.rm = TRUE), "\n")
cat("Range:", range(maine_runners$Time..minutes., na.rm = TRUE), "\n")
cat("Median:", median(maine_runners$Time..minutes., na.rm = TRUE), "\n")
cat("IQR:", IQR(maine_runners$Time..minutes., na.rm = TRUE), "\n")

# Summary statistics for Away runners
cat("\n=== Summary Statistics for Away Runners (Time in minutes) ===\n")
cat("Mean:", mean(away_runners$Time..minutes., na.rm = TRUE), "\n")
cat("Standard Deviation:", sd(away_runners$Time..minutes., na.rm = TRUE), "\n")
cat("Range:", range(away_runners$Time..minutes., na.rm = TRUE), "\n")
cat("Median:", median(away_runners$Time..minutes., na.rm = TRUE), "\n")
cat("IQR:", IQR(away_runners$Time..minutes., na.rm = TRUE), "\n")

# Determine common scale for histograms
time_range <- range(roadrace$Time..minutes., na.rm = TRUE)
breaks_seq <- seq(
    from = floor(time_range[1]),
    to = ceiling(time_range[2]),
    by = 2
)

png("barplot_timeDistribution.png", width = 1200, height = 600)

# Create histograms side by side
par(mfrow = c(1, 2))

# Histogram for Maine runners
hist(maine_runners$Time..minutes.,
    breaks = breaks_seq,
    main = "Time Distribution - Maine Runners",
    xlab = "Time (minutes)",
    ylab = "Frequency",
    col = "lightblue",
    xlim = time_range,
    ylim = c(0, 800)
)

# Histogram for Away runners
hist(away_runners$Time..minutes.,
    breaks = breaks_seq,
    main = "Time Distribution - Away Runners",
    xlab = "Time (minutes)",
    ylab = "Frequency",
    col = "lightcoral",
    xlim = time_range,
    ylim = c(0, 800)
)
dev.off() # Close and save the file

# ============================================================
# QUESTION 1(c): Side-by-side boxplots for time by location
# ============================================================

png("boxplot_timeDistribution.png", width = 1200, height = 600)
boxplot(Time..minutes. ~ Maine,
    data = roadrace,
    main = "Comparison of Race Times by Runner Location",
    xlab = "Runner Location",
    ylab = "Time (minutes)",
    col = c("lightcoral", "lightblue"),
    horizontal = FALSE
)

# Add grid for easier reading
grid()

dev.off() # Close and save the file

# ============================================================
# QUESTION 1(d): Side-by-side boxplots for age by sex
# ============================================================

# Summary statistics for Male runners
male_runners <- roadrace[roadrace$Sex == "M", ]
female_runners <- roadrace[roadrace$Sex == "F", ]

cat("\n=== Summary Statistics for Male Runners (Age in years) ===\n")
cat("Mean:", mean(male_runners$Age, na.rm = TRUE), "\n")
cat("Standard Deviation:", sd(male_runners$Age, na.rm = TRUE), "\n")
cat("Range:", range(male_runners$Age, na.rm = TRUE), "\n")
cat("Median:", median(male_runners$Age, na.rm = TRUE), "\n")
cat("IQR:", IQR(male_runners$Age, na.rm = TRUE), "\n")

cat("\n=== Summary Statistics for Female Runners (Age in years) ===\n")
cat("Mean:", mean(female_runners$Age, na.rm = TRUE), "\n")
cat("Standard Deviation:", sd(female_runners$Age, na.rm = TRUE), "\n")
cat("Range:", range(female_runners$Age, na.rm = TRUE), "\n")
cat("Median:", median(female_runners$Age, na.rm = TRUE), "\n")
cat("IQR:", IQR(female_runners$Age, na.rm = TRUE), "\n")

png("boxplot_comparison_runner_age_by_sex.png", width = 1200, height = 600)
# Create side-by-side boxplots
boxplot(Age ~ Sex,
    data = roadrace,
    main = "Comparison of Runner Ages by Sex",
    xlab = "Sex",
    ylab = "Age (years)",
    col = c("pink", "lightblue"),
    horizontal = FALSE
)

# Add grid for easier reading
grid()

dev.off() # Close and save the file

# Question 2
# Analysis of Fatal Motorcycle Accidents in South Carolina (2009)

# Read the dataset
motorcycle <- read.csv("motorcycle.csv")

# View the first few rows
head(motorcycle)

# Remove counties with 0 accidents (OTHER and UNKNOWN)
motorcycle_filtered <- motorcycle[motorcycle$Fatal.Motorcycle.Accidents > 0, ]

# ============================================
# Create boxplot
# ============================================
png("boxplot_Fatal_Motorcycle_Accidents.png", width = 1200, height = 600)

boxplot(motorcycle_filtered$Fatal.Motorcycle.Accidents,
        main = "Fatal Motorcycle Accidents by County in South Carolina (2009)",
        ylab = "Number of Fatal Accidents",
        col = "lightblue",
        border = "darkblue")

# Add points to show individual counties
points(rep(1, nrow(motorcycle_filtered)), 
       motorcycle_filtered$Fatal.Motorcycle.Accidents,
       pch = 16, col = "red", cex = 0.7)

dev.off() # Close and save the file

# ============================================
# Summary Statistics
# ============================================
cat("\n===== SUMMARY STATISTICS =====\n")
cat("Mean:", mean(motorcycle_filtered$Fatal.Motorcycle.Accidents), "\n")
cat("Median:", median(motorcycle_filtered$Fatal.Motorcycle.Accidents), "\n")
cat("Standard Deviation:", sd(motorcycle_filtered$Fatal.Motorcycle.Accidents), "\n")
cat("Range:", range(motorcycle_filtered$Fatal.Motorcycle.Accidents), "\n")
cat("IQR (Interquartile Range):", IQR(motorcycle_filtered$Fatal.Motorcycle.Accidents), "\n")
cat("Q1 (25th percentile):", quantile(motorcycle_filtered$Fatal.Motorcycle.Accidents, 0.25), "\n")
cat("Q3 (75th percentile):", quantile(motorcycle_filtered$Fatal.Motorcycle.Accidents, 0.75), "\n")
cat("Min:", min(motorcycle_filtered$Fatal.Motorcycle.Accidents), "\n")
cat("Max:", max(motorcycle_filtered$Fatal.Motorcycle.Accidents), "\n")

# Five number summary
cat("\nFive Number Summary:\n")
print(fivenum(motorcycle_filtered$Fatal.Motorcycle.Accidents))

# ============================================
# Identify Outliers
# ============================================
Q1 <- quantile(motorcycle_filtered$Fatal.Motorcycle.Accidents, 0.25)
Q3 <- quantile(motorcycle_filtered$Fatal.Motorcycle.Accidents, 0.75)
IQR_value <- IQR(motorcycle_filtered$Fatal.Motorcycle.Accidents)

# Calculate outlier boundaries
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

cat("\n===== OUTLIER ANALYSIS =====\n")
cat("Lower bound for outliers:", lower_bound, "\n")
cat("Upper bound for outliers:", upper_bound, "\n")

# Identify outliers
outliers <- motorcycle_filtered[motorcycle_filtered$Fatal.Motorcycle.Accidents > upper_bound | 
                                 motorcycle_filtered$Fatal.Motorcycle.Accidents < lower_bound, ]

cat("\nCounties identified as outliers:\n")
print(outliers)

# Sort to see highest values
cat("\nTop 5 counties with most fatal accidents:\n")
top_counties <- motorcycle_filtered[order(-motorcycle_filtered$Fatal.Motorcycle.Accidents), ][1:5, ]
print(top_counties)