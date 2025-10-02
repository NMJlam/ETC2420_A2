# Statistical Thinking (ETC2420/ETC5242) Assignment 2 Scaffold
## Pedestrian Traffic Analysis in Melbourne

---

## Initial Setup

```r
# Load required libraries
library(tidyverse)
library(MASS)       # for fitdistr()
library(gridExtra)  # for grid.arrange()
library(broom)      # for tidy()

# Set seed for reproducibility
set.seed(24205242)  # Use your student ID

# Set knitr options
knitr::opts_chunk$set(echo = TRUE, warning = FALSE,
                      eval = TRUE, message = FALSE)

# Read data (adjust path as needed)
pedestrian <- read_csv("data/pedestrian.csv")
```

---

## TASK 1: Distribution and Model Fitting

### Approach Overview
**Source:** Week 2 (Probability distributions), Week 6 (Maximum Likelihood)

### Step 1: Data Exploration and Visualization

```r
# Create histogram with smoothed density overlay
# Following Week 5 solutions pattern for dataplot function
dataplot <- function(x, bins = 50, colour = "blue") {
  ggplot(tibble(x = x), aes(x = x, y = after_stat(density))) +
    geom_histogram(bins = bins, colour = colour, fill = colour, alpha = 0.2) +
    geom_density(colour = colour, fill = colour, alpha = 0.2) +
    theme_bw()
}

# Apply to each location
location1_plot <- dataplot(pedestrian$location1, bins = 30) +
  ggtitle("Location 1: Pedestrian Counts")

# Calculate descriptive statistics (Week 3, slide 11)
summary_stats <- pedestrian %>%
  summarise(
    mean = mean(location1),
    median = median(location1),
    sd = sd(location1),
    var = var(location1)
  )
```

### Step 2: Model Selection

**Justification Framework:**
- **Count data distributions** (Week 2, slides 46-52):
  - Poisson: For count data with equal mean and variance
  - Negative Binomial: For overdispersed count data (variance > mean)

```r
# Check for overdispersion
mean(pedestrian$location1)
var(pedestrian$location1)

# If var ≈ mean → Poisson
# If var >> mean → Negative Binomial
```

### Step 3: Model Fitting

**Main Approach: Using fitdistr() (Week 6 solutions)**

```r
# Fit Poisson distribution
pois_fit <- fitdistr(pedestrian$location1, "Poisson")
pois_fit |> tidy()

# Alternative: Fit Negative Binomial
nb_fit <- fitdistr(pedestrian$location1, "negative binomial")
nb_fit |> tidy()
```

### Step 4: Model Assessment

```r
# Q-Q plot for model checking (Week 6 solutions, Exercise 23)
params_pois <- fitdistr(pedestrian$location1, "Poisson")$estimate

ggplot(tibble(x = pedestrian$location1)) +
  aes(sample = x) +
  stat_qq(distribution = qpois, dparams = params_pois) +
  stat_qq_line(distribution = qpois, dparams = params_pois) +
  theme_bw() +
  xlab("Theoretical quantiles (Poisson)") +
  ylab("Sample quantiles")
```

**Technical Justification:**
- Explain why chosen distribution fits based on overdispersion test
- Reference course material on count data distributions

**Non-technical Summary:**
"The pedestrian traffic follows a [Poisson/Negative Binomial] pattern, meaning..."

---

## TASK 2: Quantile Estimation (90th Percentile)

### Approach Overview
**Source:** Week 3 (Estimation), Week 5 (Bootstrap), Week 7 (Bayesian credible intervals)

### Method 1: Sample Quantile with Bootstrap CI

```r
# Point estimate (Week 5 solutions, Exercise 10)
q90_sample <- quantile(pedestrian$location1, 0.90)

# Bootstrap CI for quantile (Week 5 solutions, Exercise 11-12)
B <- 5000
q90_boot <- rep(NA, B)

for(b in 1:B) {
  x_b <- sample(pedestrian$location1, replace = TRUE)
  q90_boot[b] <- quantile(x_b, 0.90)
}

# 95% CI using percentile bootstrap
boot_ci <- quantile(q90_boot, c(0.025, 0.975))

# Visualize bootstrap distribution
dataplot(q90_boot, bins = 100) +
  geom_vline(xintercept = boot_ci, colour = "magenta", linetype = 2) +
  ggtitle("Bootstrap distribution of 90th percentile")
```

### Method 2: Model-Based Quantile

```r
# Using fitted Poisson model (if chosen in Task 1)
lambda_est <- pois_fit$estimate

# Calculate 90th percentile from theoretical distribution
q90_model <- qpois(0.90, lambda = lambda_est)

# CI using parametric bootstrap
lambda_boot <- rep(NA, B)
q90_model_boot <- rep(NA, B)

for(b in 1:B) {
  # Resample data
  x_b <- sample(pedestrian$location1, replace = TRUE)
  # Refit model
  lambda_boot[b] <- fitdistr(x_b, "Poisson")$estimate
  # Calculate quantile from refitted model
  q90_model_boot[b] <- qpois(0.90, lambda = lambda_boot[b])
}

model_ci <- quantile(q90_model_boot, c(0.025, 0.975))
```

**Justification:**
- Sample quantile: Direct, non-parametric, no distributional assumptions
- Model-based: Leverages fitted distribution, more efficient if model is correct

---

## TASK 3: Comparing Two Means

### Approach Overview
**Source:** Week 3 (slides 61), Week 4 (two-sample tests), Week 5 (bootstrap for differences)

### Parameter Definition
Let μ₁ = mean pedestrian count at Location 1
Let μ₂ = mean pedestrian count at Location 2
Parameter of interest: δ = μ₁ - μ₂

### Main Approach: Two-Sample t-test with CLT

```r
# Two-sample t-test (Week 4, slide 44)
t_test_result <- t.test(pedestrian$location1, pedestrian$location2)
t_test_result$conf.int

# Extract CI
ci_difference <- t_test_result$conf.int
```

### Alternative: Bootstrap CI for Difference

```r
# Bootstrap for difference in means (Week 5, slide 30)
B <- 5000
diff_boot <- rep(NA, B)

for(b in 1:B) {
  # Resample from each location independently
  x1_b <- sample(pedestrian$location1, replace = TRUE)
  x2_b <- sample(pedestrian$location2, replace = TRUE)
  diff_boot[b] <- mean(x1_b) - mean(x2_b)
}

boot_ci_diff <- quantile(diff_boot, c(0.025, 0.975))
```

### CLT Discussion (REQUIRED)

**CLT Conditions (Week 3, slides 45-48):**
```r
# Check sample sizes
n1 <- length(pedestrian$location1)
n2 <- length(pedestrian$location2)

# CLT applies if n ≥ 30 (rule of thumb from Week 3)
# Or if data is approximately normal
```

**Assessment:**
- "The CLT requires sufficiently large samples or approximately normal data..."
- "With n₁ = [value] and n₂ = [value], the CLT conditions are [met/questionable]..."
- "The bootstrap provides a distribution-free alternative..."

**Interpretation:**
"The difference in average pedestrian counts between locations is estimated as [value], with 95% CI [lower, upper]. This suggests..."

---

## TASK 4: Multiple Interval Estimates

### Approach Overview
**Source:** Week 3 (CIs for single mean), Week 1 (ggplot visualization)

### Calculate CIs for Each Location

```r
# Method 1: Using t.test for each location
locations <- list(
  loc1 = pedestrian$location1,
  loc2 = pedestrian$location2,
  loc3 = pedestrian$location3
)

# Calculate CIs
ci_results <- map_df(locations, function(x) {
  result <- t.test(x)
  tibble(
    mean = mean(x),
    ci_lower = result$conf.int[1],
    ci_upper = result$conf.int[2]
  )
}, .id = "location")
```

### Visualization (Following Week 1 solutions pattern)

```r
# Create visualization with CIs
ggplot(ci_results, aes(x = location, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2) +
  theme_bw() +
  ggtitle("95% CIs for Mean Pedestrian Counts by Location") +
  ylab("Mean Count") +
  xlab("Location")
```

### Alternative Visualization: Side-by-side boxplots with CIs

```r
# Reshape data for plotting
pedestrian_long <- pedestrian %>%
  pivot_longer(cols = c(location1, location2, location3),
               names_to = "location",
               values_to = "count")

# Boxplot with overlaid CIs
ggplot(pedestrian_long, aes(x = location, y = count)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(data = ci_results, aes(y = mean), 
             colour = "red", size = 3) +
  geom_errorbar(data = ci_results, 
                aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, colour = "red") +
  theme_bw()
```

**Interpretation:**
- Compare overlapping vs non-overlapping intervals
- Discuss practical significance of differences

---

## TASK 5: Revenue Estimation

### Problem Setup
- Revenue = $X per pedestrian above threshold c
- p = P(Count > c)
- Expected daily revenue per location = p × X × expected_daily_traffic

### Method 1: Empirical Proportion

```r
# Set threshold (example: c = 100)
threshold <- 100
revenue_per_person <- 10  # $10 per person above threshold

# Calculate empirical proportion
p_empirical <- mean(pedestrian$location1 > threshold)

# Bootstrap CI for proportion
B <- 5000
p_boot <- rep(NA, B)

for(b in 1:B) {
  x_b <- sample(pedestrian$location1, replace = TRUE)
  p_boot[b] <- mean(x_b > threshold)
}

p_ci <- quantile(p_boot, c(0.025, 0.975))

# Revenue calculation
expected_revenue <- p_empirical * revenue_per_person * mean(pedestrian$location1)
```

### Method 2: Model-Based Probability

```r
# Using fitted Poisson model from Task 1
lambda_est <- pois_fit$estimate

# Calculate P(X > c) from model
p_model <- 1 - ppois(threshold, lambda = lambda_est)

# Bootstrap CI for model-based probability
p_model_boot <- rep(NA, B)

for(b in 1:B) {
  x_b <- sample(pedestrian$location1, replace = TRUE)
  lambda_b <- fitdistr(x_b, "Poisson")$estimate
  p_model_boot[b] <- 1 - ppois(threshold, lambda = lambda_b)
}

p_model_ci <- quantile(p_model_boot, c(0.025, 0.975))
```

### Revenue CI Calculation

```r
# Convert probability CI to revenue CI
revenue_ci_lower <- p_ci[1] * revenue_per_person * mean(pedestrian$location1)
revenue_ci_upper <- p_ci[2] * revenue_per_person * mean(pedestrian$location1)

# Create summary table
revenue_summary <- tibble(
  method = c("Empirical", "Model-based"),
  p_estimate = c(p_empirical, p_model),
  revenue_estimate = c(expected_revenue, 
                      p_model * revenue_per_person * mean(pedestrian$location1)),
  ci_lower = c(revenue_ci_lower, ...),
  ci_upper = c(revenue_ci_upper, ...)
)
```

**Justification:**
- Empirical: Direct from data, no assumptions
- Model-based: Leverages distributional fit, smoother estimates

---

## General Report Structure

### 1. Introduction
- Brief context about pedestrian traffic analysis
- Overview of objectives

### 2. Data Description
- Summary statistics table
- Initial visualizations

### 3. Task Results
For each task:
- **Technical explanation** with code and statistical reasoning
- **Non-technical summary** for stakeholders
- **Visualizations** with proper labels

### 4. Conclusions and Recommendations
- Key findings across all tasks
- Practical implications for city planning/business

### 5. Appendix
- Additional diagnostic plots
- Sensitivity analyses
- Full code listing

---

## Key Reminders

### From Course Materials:

1. **Always justify** statistical choices with references to lectures/tutorials
2. **Use both** technical and non-technical language
3. **Follow code patterns** from provided solutions exactly
4. **Include diagnostic checks** for all models
5. **Set seed** for reproducibility
6. **Label all plots** clearly with titles and axis labels

### Common Methods by Week:

- **Week 1:** Data wrangling, ggplot2 basics
- **Week 2:** Probability distributions (Poisson, Negative Binomial)
- **Week 3:** Estimation, CIs, CLT
- **Week 4:** Hypothesis testing, t.test(), prop.test()
- **Week 5:** Bootstrap (percentile method)
- **Week 6:** Maximum likelihood, fitdistr()
- **Week 7:** Bayesian methods (if applicable)
- **Week 8:** Decision theory (if revenue optimization needed)
- **Week 9:** Regression (if covariates available)

### R Function Quick Reference:

```r
# Frequently used functions from course
fitdistr()        # MLE fitting (Week 6)
t.test()          # CIs and testing (Week 3-4)
quantile()        # Empirical quantiles (Week 5)
qpois(), ppois()  # Theoretical quantiles/probabilities
sample()          # Bootstrap resampling
ggplot()          # All visualizations
tidy()            # Clean output formatting
```
