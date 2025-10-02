---
title: "Week 5, Solutions"
subtitle: "Statistical Thinking (ETC2420 / ETC5242)"
date-format: "[Semester 2,] YYYY"
date: "2025"
toc: true
format:
  html:
    embed-resources: true
---

```{r}
#| message: false
library(tidyverse)
set.seed(24205242)
```

## Exercise 1

1.  Complete the code below to define a working function.

```{r}
# Creates a histogram and smoothed histogram given a vector of values.
dataplot <- function(x, bins = 50, colour = "blue") {
  ggplot(tibble(x = x), aes(x = x, y = after_stat(density))) +
    geom_histogram(bins = bins, colour = colour, fill = colour, alpha = 0.2) +
    geom_density(colour = colour, fill = colour, alpha = 0.2) +
    theme_bw()
}
```

## Exercises 2--9

2.  Read in the `CBT.csv` data file into a tibble called `CBT`.

```{r}
#| message: false
CBT <- read_csv("data/CBT.csv")
```

3.  Create a new column called `Diff` which is the difference in scores for
    each subject, in the same way as in the Week 3 tutorial.

```{r}
CBT <- CBT |> mutate(Diff = score2 - score1)
```

4.  Draw a histogram of these differences, overlayed with a smoothed histogram.

We can simply use the `dataplot()` function we wrote earlier. Given the small
sample size, using a smaller number of bins than the default would be a good
idea.

```{r}
dataplot(CBT$Diff, bins = 15)
```

5.  Is it possible to also overlay the population distribution on this plot? If
    yes, then do so. If not, then explain why.

No, we don't know the population distribution so we are not able to draw it.

6.  Calculate the 95% confidence interval for $\delta$ using methods you learnt
    in Week 3. (This is for comparing against the bootstrap method, which we do
    below.)

```{r}
CLT.CI <- t.test(CBT$Diff)$conf.int
CLT.CI
```

7.  Starting with the `Diff` variable, generate $B = 5000$ bootstrap samples.
    For each bootstrap sample, calculate and save the sample mean, storing them
    in a vector (that vector should be of length 5000, with one value per
    bootstrap sample). You will need this vector for later exercises. Remember
    to set a seed so that your bootstrap samples are reproducible.

```{r}
# Note: a seed was set at the top of this document.

# Number of bootstrap samples.
B <- 5000

# Initialise vectors to store statistics from bootstrap samples.
xbar_boot <- rep(NA, B)  # for Exercise  7
xmed_boot <- rep(NA, B)  # for Exercise 11
xsd_boot  <- rep(NA, B)  # for Exercise 16

# Generate the bootstrap samples and calculate statistics.
for(b in 1:B) {
  x_b <- sample(CBT$Diff, size = length(CBT$Diff), replace = TRUE)
  xbar_boot[b] <- mean(x_b)    # for Exercise  7
  xmed_boot[b] <- median(x_b)  # for Exercise 11
  xsd_boot[b]  <- sd(x_b)      # for Exercise 16
}
```

8.  Calculate a 95% confidence interval for $\delta$ from your bootstrap
    samples, using the percentile bootstrap method. (Hint: use the `quantile()`
    function.)

```{r}
boot.CI <- quantile(xbar_boot, c(0.025, 0.975))
boot.CI
```

9.  The $B$ sample means of the bootstrap samples provide us with an empirical
    estimate of the sampling distribution of the `Diff` statistic. Let's
    visualise it. Draw a histogram of these $B$ values and overlay it with a
    smoothed histogram. Then also add the following elements:

    -   Vertical lines to show the lower and upper bounds of the 95% confidence
        intervals you calculated above. Use different colours and line styles
        to distinguish between the bootstrap-based CI (from Exercise 8) and the
        one you calculated earlier (from Exercise 6).
    -   An appropriate title and axis labels.

```{r}
dataplot(xbar_boot, bins = 100) +
  geom_vline(xintercept = boot.CI, linetype = 5, colour = "magenta") +
  geom_vline(xintercept =  CLT.CI, linetype = 2, colour = "darkslategrey") +
  annotate("text", label = round(boot.CI[1], 2), x = (boot.CI[1] - 0.5), y = 0.5, colour = "magenta") +
  annotate("text", label = round(boot.CI[2], 2), x = (boot.CI[2] + 0.5), y = 0.5, colour = "magenta") +
  annotate("text", label = round(CLT.CI[1],  2), x =  (CLT.CI[1] - 0.5), y = 0.75, colour = "darkslategrey") +
  annotate("text", label = round(CLT.CI[2],  2), x =  (CLT.CI[2] + 0.5), y = 0.75, colour = "darkslategrey") +
  xlab(expression(bar(x))) +
  ggtitle(expression(paste("Approximate sampling distribution of ", bar(X), " (bootstrap with B = 5000)")))
```

## Exercises 10--14

10. Calculate a point estimate of $m$.

```{r}
median(CBT$Diff)
```

11. Add extra code to your answer for Exercise 7 (and re-run it) so that you
    can use the same bootstrap samples to generate a CI for $m$. You simply
    need to calculate the new statistic and store it.

(See the code in the solution for Exercise 7.)

12. Calculate a 95% confidence interval for $m$ from your bootstrap samples,
    using the percentile bootstrap method.

```{r}
boot.CI <- quantile(xmed_boot, c(0.025, 0.975))
boot.CI
```

13. Recreate the plot from Exercise 9, but now for inferring $m$. This time,
    you should only show the bootstrap-based CI.

```{r}
dataplot(xmed_boot, bins = 40) +
  geom_vline(xintercept = boot.CI, linetype = 5, colour = "magenta") +
  annotate("text", label = round(boot.CI[1], 2), x = (boot.CI[1] - 0.5), y = 0.5, colour = "magenta") +
  annotate("text", label = round(boot.CI[2], 2), x = (boot.CI[2] + 0.5), y = 0.5, colour = "magenta") +
  xlab("Median of score difference") +
  ggtitle("Approximate sampling distribution of the median (bootstrap with B = 5000)")
```

14. Describe the approximate sampling distribution. What interesting features
    do you notice?

The approximate sampling distribution is very 'lumpy'. There only a relatively
small number of different possible values, so it looks more like a discrete
distribution than a continuous one. This is partly due to the original data
having many repeated values (e.g., $-3.41$ appears 5 times), and partly due to
the original data being relatively small (sample size of 60) and us using a
statistic (median) that typically evaluates to the value of one of the data
points. In contrast, a statistic such as the sample mean or sample standard
deviation will tend to give a greater variety of values across all of the
bootstrap samples.

In the plot for Exercise 13, we have used a smaller number of bins in the
histogram to avoid showing this 'lumpiness'. This is appropriate if you are
aware of this feature and don't want it to be distraction when presenting the
results. However, when you are doing data analysis it is helpful to see and
notice such features, so you can spot any data errors or issues, and then
decide whether or not it is something that needs further attention. In this
case, it's not a data error so we don't need to worry about it.

Note that the smoothed histogram is still showing the lumpiness. To change this
would require going back and changing the `dataplot()` function, which you
might like to try on your own.

## Exercises 15--16

15. What would be an appropriate statistic to use for this parameter?

The sample standard deviation of the pairwise differences.

16. Repeat Exercises 10--14 to perform inference for $\sigma$ using your chosen
    statistic.

```{r}
sd(CBT$Diff)
```

```{r}
boot.CI <- quantile(xsd_boot, c(0.025, 0.975))
boot.CI
```

```{r}
dataplot(xsd_boot, bins = 100) +
  geom_vline(xintercept = boot.CI, linetype = 5, colour = "magenta") +
  annotate("text", label = round(boot.CI[1], 2), x = (boot.CI[1] - 0.5), y = 0.5, colour = "magenta") +
  annotate("text", label = round(boot.CI[2], 2), x = (boot.CI[2] + 0.5), y = 0.5, colour = "magenta") +
  xlab("Standard deviation of the difference in scores") +
  ggtitle("Approximate sampling distribution of the st. dev. (bootstrap with B = 5000)")
```

This approximate sampling distribution looks much smoother (compared to the one
for the median).
