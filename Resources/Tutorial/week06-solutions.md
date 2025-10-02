---
title: "Week 6, Solutions"
subtitle: "Statistical Thinking (ETC2420 / ETC5242)"
date-format: "[Semester 2,] YYYY"
date: "2025"
toc: true
format:
  html:
    embed-resources: true
---

```{r}
#| label: setup
#| message: false
library(tidyverse)
library(broom)
library(gridExtra)
library(MASS)
```

# Normal distribution

```{r}
set.seed(24205242)
x_norm <- rnorm(n = 36, mean = 10, sd = 2)
```

```{r}
normal_fit <- fitdistr(x_norm, "normal")
normal_fit |> tidy()
```

## Exercise 1

1.  Create the file `dataplot.R` and put the definition of the `dataplot()`
    function inside it.

The file should contain the following code:

```{r}
# Creates a histogram and smoothed histogram given a vector of values.
dataplot <- function(x, bins = 50, colour = "blue") {
  ggplot(tibble(x = x), aes(x = x, y = after_stat(density))) +
    geom_histogram(bins = bins, colour = colour, fill = colour, alpha = 0.2) +
    geom_density(colour = colour, fill = colour, alpha = 0.2) +
    theme_bw()
}
```

## Exercises 2--4

2.  Use the output from `fitdistr()`, along with appropriate quantiles from a
    normal distribution, to calculate approximate 95% confidence intervals for
    $\mu$ and $\sigma$. Use the fact that we know the MLE is asymptotically
    normally distributed.

```{r}
normal_mle <- normal_fit$estimate  # point estimates
normal_se  <- normal_fit$sd        # standard errors
normal_ci_lower <- normal_mle + qnorm(0.025) * normal_se
normal_ci_upper <- normal_mle + qnorm(0.975) * normal_se

# Put the values together into a tibble, for convenient presentation.
tibble(parameter = c("mu", "sigma"),
       mle = normal_mle,
       se  = normal_se,
       ci_lower = normal_ci_lower,
       ci_upper = normal_ci_upper)
```

Notice that the code does the calculations for each parameter at the same time,
because each object is a $2 \times 1$ vector.

3.  Use the bootstrap to calculate a 95% confidence interval for each
    parameter. Use $B = 5000$ bootstrap samples.

```{r}
# Initialise values for doing the bootstrap.
B <- 5000
normal_boot <- matrix(nrow = B, ncol = 2)

# Generate the bootstrap samples and calculate statistics.
for(b in 1:B) {
  temp <- sample(x_norm, replace = TRUE)
  normal_boot[b,] <- fitdistr(temp, "normal")$estimate
}

# Calculate CIs.
normal_boot_ci_mu    <- quantile(normal_boot[, 1], c(0.025, 0.975))
normal_boot_ci_sigma <- quantile(normal_boot[, 2], c(0.025, 0.975))

# Look at the values.
normal_boot_ci_mu
normal_boot_ci_sigma
```

The 95% confidence interval for $\mu$ is $(9.56, 10.96)$.

The 95% confidence interval for $\sigma$ is $(1.62, 2.72)$.

4.  Visualise the approximate sampling distributions, as given by the bootstrap
    samples, and include vertical lines to show the bounds of the 95%
    confidence intervals. (This is similar to what we did in Week 5.)

```{r}
#| fig-height: 3
p_mu <- dataplot(normal_boot[, 1], bins = 100) +
  geom_vline(xintercept = normal_boot_ci_mu, colour = "magenta") +
  xlab(expression(mu))
p_sigma <- dataplot(normal_boot[, 2], bins = 100) +
  geom_vline(xintercept = normal_boot_ci_sigma, colour = "magenta") +
  xlab(expression(sigma))
grid.arrange(p_mu, p_sigma, ncol = 2)
```

# Beta distribution

## Exercises 5--10

5.  Simulate the data. Remember to set a seed to make it reproducible.

```{r}
set.seed(24205242)
x_beta <- rbeta(100, shape1 = 2, shape2 = 4)
```

6.  Visualise the sample using an appropriate plot.

```{r}
dataplot(x_beta, bins = 30) +
  ggtitle("Histogram of simulated sample from Beta(2, 4)") +
  xlab("Observations (simulated)")
```

7.  Calculate MLEs and standard errors for the parameters, $\alpha$ and
    $\beta$.

Use the `start` argument to specify initial values of $\alpha = 1$ and
$\beta = 1$ for the optimisation algorithm.

```{r}
#| warning: false
beta_fit <- fitdistr(x_beta, "beta", start = list(shape1 = 1, shape2 = 1))
beta_fit |> tidy()
```

The estimates are $\hat\alpha = 2.1$ and $\hat\beta = 4.1$. The standard errors
are $\mathrm{se}(\hat\alpha) = 0.28$ and $\mathrm{se}(\hat\beta) = 0.57$.

8.  Calculate a CLT-based 95% confidence interval for each parameter.

```{r}
beta_mle <- beta_fit$estimate  # point estimates
beta_se  <- beta_fit$sd        # standard errors
beta_ci_lower <- beta_mle + qnorm(0.025) * beta_se
beta_ci_upper <- beta_mle + qnorm(0.975) * beta_se

# Put the values together into a tibble, for convenient presentation.
tibble(parameter = c("alpha", "beta"),
       mle = beta_mle,
       se  = beta_se,
       ci_lower = beta_ci_lower,
       ci_upper = beta_ci_upper)
```

The 95% confidence interval for $\alpha$ is $(1.55, 2.64)$.

The 95% confidence interval for $\beta$ is $(2.96, 5.22)$.

9.  Use the bootstrap method to calculate 95% confidence intervals for the
    parameters.

```{r}
#| warning: false
# Initialise values for doing the bootstrap.
B <- 5000
beta_boot <- matrix(nrow = B, ncol = 2)

# Generate the bootstrap samples and calculate statistics.
for(b in 1:B) {
  temp <- sample(x_beta, replace = TRUE)
  beta_boot[b,] <- fitdistr(temp, "beta",
                            start = list(shape1 = 1, shape2 = 1))$estimate
}

# Calculate CIs.
beta_boot_ci_alpha <- quantile(beta_boot[, 1], c(0.025, 0.975))
beta_boot_ci_beta  <- quantile(beta_boot[, 2], c(0.025, 0.975))

# Look at the values.
beta_boot_ci_alpha
beta_boot_ci_beta
```

The 95% confidence interval for $\alpha$ is $(1.64, 2.85)$.

The 95% confidence interval for $\beta$ is $(3.16, 5.64)$.

There seems to be a bit more of a difference between the CLT and bootstrap CIs.
Looking at the sampling distributions below, this is probably due to the
skewness inherent in the sampling distributions. The CLT-based CIs do not
incorporate this.

10. Visualise the approximate sampling distributions, as given by the bootstrap
    samples, and include vertical lines to show the bounds of the 95%
    confidence intervals.

```{r}
#| fig-height: 3
p_alpha <- dataplot(beta_boot[, 1], bins = 100) +
  geom_vline(xintercept = beta_boot_ci_alpha, colour = "magenta") +
  xlab(expression(alpha))
p_beta <- dataplot(beta_boot[, 2], bins = 100) +
  geom_vline(xintercept = beta_boot_ci_beta, colour = "magenta") +
  xlab(expression(beta))
grid.arrange(p_alpha, p_beta, ncol = 2)
```

# Poisson distribution

## Exercises 11--16

11. Simulate the data. Remember to set a seed to make it reproducible.

```{r}
set.seed(24205242)
x_pois <- rpois(75, lambda = 5)
```

12. Visualise the sample using an appropriate plot.

```{r}
dataplot(x_pois, bins = 30) +
  ggtitle("Histogram of simulated sample from Poisson(5)") +
  xlab("Observations (simulated)")
```

13. Calculate the MLE and standard error of $\lambda$.

```{r}
pois_fit <- fitdistr(x_pois, "poisson")
pois_fit |> tidy()
```

We have $\hat\lambda = 5.0$ and $\mathrm{se}(\hat\lambda) = 0.26$.

14. Calculate a CLT-based 95% confidence interval for $\lambda$.

```{r}
pois_mle <- pois_fit$estimate  # point estimate
pois_se  <- pois_fit$sd        # standard error
pois_ci_lower <- pois_mle + qnorm(0.025) * pois_se
pois_ci_upper <- pois_mle + qnorm(0.975) * pois_se

# Put the values together into a tibble, for convenient presentation.
tibble(parameter = c("lambda"),
       mle = pois_mle,
       se  = pois_se,
       ci_lower = pois_ci_lower,
       ci_upper = pois_ci_upper)
```

The 95% confidence interval for $\lambda$ is $(4.5, 5.5)$.

15. Use the bootstrap method to calculate a 95% confidence interval for
    $\lambda$.

```{r}
# Initialise values for doing the bootstrap.
B <- 5000
pois_boot <- matrix(nrow = B, ncol = 2)

# Generate the bootstrap samples and calculate statistics.
for(b in 1:B) {
  temp <- sample(x_pois, replace = TRUE)
  pois_boot[b,] <- fitdistr(temp, "Poisson")$estimate
}

# Calculate CIs.
pois_boot_ci_lambda <- quantile(pois_boot[, 1], c(0.025, 0.975))

# Look at the values.
pois_boot_ci_lambda
```

The 95% confidence interval for $\lambda$ is $(4.5, 5.5)$.

These are very similar to the CLT-based CIs. The Poisson distribution is used
for count data, which is discrete. There is less variation in the data, which
may contribute to the similarity between the intervals.

16. Visualise the approximate sampling distribution, as given by the bootstrap
    samples, and include vertical lines to show the bounds of the 95%
    confidence interval.

```{r}
p_lambda <- dataplot(pois_boot[, 1], bins = 100) +
  geom_vline(xintercept = pois_boot_ci_lambda, colour = "magenta") +
  xlab(expression(lambda))
p_lambda
```

# CBT data

```{r}
#| eval: true
#| message: false
CBT <- read_csv("data/CBT.csv")
x_diff <- CBT$score2 - CBT$score1
```

## Exercises 17--22

17. Visualise the pairwise score differences with an appropriate plot.

```{r}
dataplot(x_diff, bins = 30) +
  ggtitle("Histogram of difference in CBT scores") +
  xlab("Score difference")
```

18. What would be an appropriate distribution to fit to the score differences?

Our data are continuous, and contain both positive and negative values.

A normal distribution is a sensible choice and a good one to start with. A
t-distribution is a possible alternative.

The sample is skewed, so perhaps we could look at a skewed distribution. The
gamma or log-normal distributions will not work, because we need to allow for
negative values. We haven't learnt about other distributions that are both
skewed and allow negative values, so we will stick with a normal distribution
for this exercise.

19. Using the distribution you chose above, fit it to the data using
    `fitdistr()`.

```{r}
cbt_normal_fit <- fitdistr(x_diff, "normal")
cbt_normal_fit |> tidy()
```

According to these MLEs, the normal distribution that provides the best fit to
the difference in CBT scores is one with a mean of $-2.35$ and a standard
deviation of $6.43$.

20. Calculate 95% confidence intervals for the parameters of the distribution
    you chose. Use both a CLT- and a bootstrap-based approach.

First doing a CLT-based approach:

```{r}
cbt_normal_mle <- cbt_normal_fit$estimate  # point estimates
cbt_normal_se  <- cbt_normal_fit$sd        # standard errors
cbt_normal_ci_lower <- cbt_normal_mle + qnorm(0.025) * cbt_normal_se
cbt_normal_ci_upper <- cbt_normal_mle + qnorm(0.975) * cbt_normal_se

# Put the values together into a tibble, for convenient presentation.
tibble(parameter = c("mu", "sigma"),
       mle = cbt_normal_mle,
       se  = cbt_normal_se,
       ci_lower = cbt_normal_ci_lower,
       ci_upper = cbt_normal_ci_upper)
```

Next doing a bootstrap:

```{r}
set.seed(24205242)

# Initialise values for doing the bootstrap.
B <- 5000
cbt_normal_boot <- matrix(nrow = B, ncol = 2)

# Generate the bootstrap samples and calculate statistics.
for(b in 1:B) {
  temp <- sample(x_diff, replace = TRUE)
  cbt_normal_boot[b,] <- fitdistr(temp, "normal")$estimate
}

# Calculate CIs.
cbt_normal_boot_ci_mu    <- quantile(cbt_normal_boot[, 1], c(0.025, 0.975))
cbt_normal_boot_ci_sigma <- quantile(cbt_normal_boot[, 2], c(0.025, 0.975))

# Look at the values.
cbt_normal_boot_ci_mu
cbt_normal_boot_ci_sigma
```

The MLE estimate for $\mu$ is $-2.35$, with a corresponding standard error of
$0.83$. This gives a CLT-based 95% confidence interval of $(-3.97, -0.72)$. The
bootstrap CI is $(-3.92, -0.70)$, which is very similar.

For $\sigma$, the corresponding CIs are $(5.28, 7.58)$ and $(4.97, 7.66)$.
These differ, most likely due to asymmetries in the sampling distribution,
which the bootstrap is able to capture.

21. Visualise the approximate sampling distributions, as given by the bootstrap
    samples, and include vertical lines to show the bounds of the 95%
    confidence intervals.

```{r}
#| fig-height: 3
p_mu <- dataplot(cbt_normal_boot[, 1], bins = 100) +
  geom_vline(xintercept = cbt_normal_boot_ci_mu, colour = "magenta") +
  xlab(expression(mu))
p_sigma <- dataplot(cbt_normal_boot[, 2], bins = 100) +
  geom_vline(xintercept = cbt_normal_boot_ci_sigma, colour = "magenta") +
  xlab(expression(sigma))
grid.arrange(p_mu, p_sigma, ncol = 2)
```

22. We will now compare these estimates with those from the Week 5 tutorial.
    Are the inference results based on the MLE similar to the results from Week
    5? If any are different, can you explain why?

The CLT-based 95% CI for the mean in Week 5 was $(-4.02, -0.67)$. This is
different to the CLT-based CI we have calculated above from the MLE.

There are two reasons for this. The MLE estimate of the sample standard
deviation divides by $n$, whereas the sample standard deviation (`sd(x)` used
in Week 5) divides by $n - 1$. Also, in Week 5 we used quantiles from a
t-distribution, so our critical value was `r round(qt(0.975, 59), 2)`, whereas
this week it is `r round(qnorm(0.975), 2)`.

The bootstrap for the mean in Week 5 was $(-3.92, -0.70)$, which is the same as
the bootstrap here. If we used the same seed, we would get identical results
because the two procedures are identical (since the MLE for $\mu$ is
$\bar{x}$).

The MLE estimate for $\sigma$ is 6.43, with a corresponding standard error of
0.59. This gave us a 95% confidence interval of $(5.28, 7.58)$. We could not
obtain a CLT-based CI for $\sigma$ in Week 5.

The bootstrap-based CI for $\sigma$ we got above was $(4.97, 7.66)$ and in Week
5 we got $(5.01, 7.72)$. There is a small difference, because of the way the
MLE calculates the standard deviation (see above), which means we aren't
actually comparing the same estimators, but the difference between them is
minor.

## Exercises 23--24

```{r}
params_norm <- fitdistr(x_diff, "normal")$estimate
ggplot(tibble(x = x_diff)) +
  aes(sample = x) +
  stat_qq(distribution = qnorm, dparams = params_norm) +
  stat_qq_line(distribution = qnorm, dparams = params_norm) +
  theme_bw() +
  xlab("Theoretical quantiles (normal distribution)") +
  ylab("Sample (CBT score difference)")
```

23. What can you conclude from the QQ plot?

For the most part the normal distribution fits our data well, but the right
tail looks like it deviates from a normal.

24. Create another QQ plot, this time for assessing whether a t-distribution is
    an adequate model.

```{r}
params_t <- fitdistr(x_diff, "t")$estimate["df"]
ggplot(tibble(x = x_diff), aes(sample = x)) +
  stat_qq(distribution = qt, dparams = params_t) +
  stat_qq_line(distribution = qt, dparams = params_t) +
  theme_bw() +
  xlab("Theoretical quantiles (t-distribution)") +
  ylab("Sample (CBT score difference)")
```

25. Do you think the t-distribution fits the data better?

It looks like it fits the right tail a bit better than the normal distribution,
but the difference between them is quite small. I expect it might not matter
much in practice.

## Exercises 26--27

```{r}
library(qqplotr)
```

```{r}
ggplot(tibble(x = x_diff)) +
  aes(sample = x) +
  stat_qq_band(distribution = "norm", dparams = params_norm) +
  stat_qq_line(distribution = "norm", dparams = params_norm) +
  stat_qq_point(distribution = "norm", dparams = params_norm) +
  theme_bw() +
  xlab("Theoretical quantiles (normal distribution)") +
  ylab("Sample (CBT score difference)")
```

26. What can you conclude from the above plot?

The QQ plot suggests that a normal distribution fits fine for most of the
values, but will struggle to adequately capture a few of the values in right
tail.

In practice, this is still a reasonable choice to use as a model, remembering
that it isn't a perfect fit and is only a model. If the application of interest
requires inference only for quantities such as the mean, then this will
suffice. However, if we needed to get accurate estimates of the right tail of
the population, then we would need to use a different model.

27. Repeat this for the t-distribution.

```{r}
ggplot(tibble(x = x_diff)) +
  aes(sample = x) +
  stat_qq_band(distribution = "t", dparams = list(df = params_t)) +
  stat_qq_line(distribution = "t", dparams = list(df = params_t)) +
  stat_qq_point(distribution = "t", dparams = list(df = params_t)) +
  theme_bw() +
  xlab("Theoretical quantiles (t-distribution)") +
  ylab("Sample (CBT score difference)")
```

This provides a better fit to the right tail than the normal distribution.
