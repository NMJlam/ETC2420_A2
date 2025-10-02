---
title: "Week 3, Solutions"
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
#| include: false
#| warning: false
#| error: false
#| message: false
knitr::opts_chunk$set(message = FALSE,
                      echo    = TRUE,
                      eval    = TRUE,
                      fig.height = 3,
                      fig.width  = 7)
```

```{r}
#| eval: true
library(tidyverse)
library(kableExtra)
library(gridExtra)
library(ggthemes)
theme_set(theme_minimal(base_size = 12))
```

# Michaelson's Speed of Light Data

```{r}
light <- as_tibble(morley)
light_summary <- light |>
    summarise(n = n(), mean = mean(Speed), median = median(Speed),
              SD = sd(Speed), IQR = IQR(Speed))
```

## Exercises 1--2

1.  Describe these data. What conclusions can you draw from the plot?

The density plots suggests that the data are multimodal (thus, probably not
described well by a normal distribution). The data from the experiments seem to
overestimate the speed of light. From the plot, we cannot easily determine if
this difference is just due to sampling variation or a systematic difference
(but later when we calculate the CI for $\mu$ we'll see that it is implausible
that the difference is due only to sampling variation).

2.  Draw a histogram of these data.

```{r}
#| warning: false
light |> ggplot(aes(Speed)) +
  geom_histogram() +
  xlab("Speed (km/s, above 299,000)")
```

## Exercises 3--6

3.  Calculate the 95% confidence interval without using `t.test()`.

We follow the same method as shown in the lectures, using the `qt()` function
to calculate the desired quantile from the t-distribution.

```{r}
light_summary$mean + c(-1, 1) * qt(0.975, light_summary$n - 1) *
  light_summary$SD / sqrt(light_summary$n)
```

This matches exactly with the output from `t.test()`.

4.  Which of the following quantities are parameters and which are statistics?

    -   $\mu$
    -   $\mu_W$
    -   $\hat\mu$

$\mu$ is a parameter, it describes the population we are studying here (the
value obtained from Michaelson's experimental setup).

$\mu_W$ is a value on the same scale as $\mu$. It isn't technically a parameter
because it doesn't describe the population in this case, but is a *reference
value* that we are using to compare to the parameter $\mu$.

$\hat\mu$ is an estimate of $\mu$, and thus is a statistic.

5.  If we were to repeat Michaelson's experiments to get another 100
    observations and used these to calculate a new 95% confidence interval,
    what is the probability that it would contain $\mu$? What about $\mu_W$?

Before actually doing the extra experiments, we know that such a CI would have
a probability of 0.95 of containing $\mu$. This is a consequence of how we
define and construct CIs.

We don't know the exact probability that it would contain $\mu_W$. However,
given what we have learnt from the initial sample, in that $\mu_W$ seems to
differ substantially to $\mu$, presumably the probability would be a very low.

6.  For the confidence interval you calculated above, what is the probability
    that it contains $\mu$?

$\mu$ is a fixed value and the realised confidence interval is also fixed.
Thus, this question does not have a probabilisitic interpretation. The answer
is either 1 or 0 (true or false), depending on whether or not $\mu$ is in the
realised interval.

# Cognitive behavioural therapy: Does the treatment have an effect?

```{r}
CBT <- read_csv("data/CBT.csv")
```

## Exercises 7--9

7.  To begin with, add a new column to `CBT` that calculates the difference
    between the two scores for each subject. Specifically, call the new column
    `Diff` and define it as `score2` minus `score1`. (Hint: use the `mutate()`
    function.)

```{r}
CBT <- CBT |> mutate(Diff = score2 - score1)
```

8.  For the first plot, we want an estimated density plot of these differences,
    similar in style to the plot we made for the speed of light data in the
    previous section. Create such a plot, saving it to an object called
    `p2density`. Include a vertical red line showing the sample mean (of
    `Diff`) and a vertical black line at zero.

```{r}
p2density <- CBT |>
  ggplot(aes(x = Diff, y = after_stat(density))) +
  geom_density(fill = "cornsilk") +
  geom_vline(xintercept = mean(CBT$Diff), colour = "red") +
  geom_vline(xintercept = 0)
```

9.  For the second plot, we want to display the distributions of the two scores
    side-by-side. To do that easily in the tidyverse, it will help if we first
    restructure the tibble so the scores are all in a single column; this is
    known as a "long" format (in contrast to a "wide" format, where such values
    are in separate columns). The `pivot_longer()` function can do this for us,
    as follows:

    ```{r}
    CBT_longer <- CBT |>
        pivot_longer(cols = 2:3, names_to = "assess", values_to = "score")
    ```

    This gives us a new tibble, with the scores together in a single column
    called `score`, and a new column called `assess` that specifies the type of
    each score (either `score1` or `score2`). Using this new tibble, create
    side-by-side violin plots to compare the distribution of the two scores.
    (Hint: use the `geom_violin()` function as part of your plotting commands.)
    Save your plot into an object called `p2violin`.

```{r}
p2violin <- CBT_longer |> ggplot(aes(x = as_factor(assess),
                                      y = score,
                                      colour = assess)) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0.1) +
  xlab("Assessment") +
  ylab("Score")
```

Display these plots side-by-side:

```{r}
grid.arrange(p2density, p2violin, ncol = 2)
```

## Exercises 10--12

10. Produce a summary tibble, based on all 60 observations of `Diff`, that
    displays the number of observations $n$, and the sample mean, median,
    standard deviation and interquartile range.

```{r}
CBT |>
  summarise(n = n(),
            mean = mean(Diff),
            median = median(Diff),
            SD = sd(Diff),
            IQR = IQR(Diff)) |>
  kable(digits = 1) |>
  kable_styling(latex_options = "hold_position")
```

11. Calculate a 95% confidence interval for `Diff`.

```{r}
t.test(CBT$Diff)$conf.int
```

The following also works and gives the same results:

```{r}
t.test(CBT$score2, CBT$score1, paired = TRUE)$conf.int
```

12. What insights can you draw your plots and data analyses?

From the plots, we see that the observed difference in scores is quite variable
and on average is slightly less than zero.

Score 2 is in general a bit lower than score 1, and is usually less variable
(more tightly clustered around a central value) however there are a few extreme
values.

The data provide evidence for a small average reduction in the score due to the
treatment, somewhere in the range of 0.67 to 4.0 lower after the treatment.

# Birth weight

```{r}
library(MASS)
data(birthwt)
```

## Exercises 13--16

13. Briefly explain what these two variables represent.

`bwt` records the birth weight (in grams) of each baby.

`smoke` records the smoking status during pregnancy of each mother, presumably
with `1` indicating smokers and `0` indicating non-smokers.

14. Produce at least one appropriate visualisation of the `bwt` variable, in
    relation to the smoking status of the mother during pregnancy. You are not
    required to produce exactly the same type of plots as above, rather produce
    what you think is of interest. (Have a go on your own to begin with. After
    that, have a look at [Section 6.4 of the R Graphics
    Cookbook](https://r-graphics.org/RECIPE-DISTRIBUTION-MULTI-DENSITY.html)
    for some more ideas.)

There are many possible choices, such as box plots, histograms, violing plots.
The solution from the Cookbook uses shaded density plots:

```{r}
# Convert smoke to a factor.
birthwt_mod <- birthwt |>
  mutate(smoke = as.factor(smoke))

# Map smoke to fill and make the fill semitransparent by setting alpha.
ggplot(birthwt_mod, aes(x = bwt, fill = smoke)) +
  geom_density(alpha = .3) 
```

15. Are the two groups independent samples? Explain why or why not.

The two groups are smokers and non-smokers. These are different women (and it
is not a before/after type study), so the two groups are independent of each
other.

16. Produce a summary tibble that displays the number of observations where the
    mother smoked (call it `n1`), and the sample mean birth weight of babies
    born to these mothers (call it `mean1`), along with the corresponding
    standard deviation (call it `SD1`). Display these in a table. Repeat this
    for the birth weights of babies born to mothers who did not smoke (with
    similar variable names but with a "0" instead of a "1"). Hint: you might
    like to use the `filter()` function.

```{r}
birthwt1 <- birthwt |>
  filter(smoke == 1) |>
  summarise(n1    = n(),
            mean1 = mean(bwt),
            SD1   = sd(bwt))
birthwt0 <- birthwt |>
  filter(smoke == 0) |>
  summarise(n0    = n(),
            mean0 = mean(bwt),
            SD0   = sd(bwt))

birthwt1 |>
  kable(digits = 0) |>
  kable_styling(latex_options = "hold_position")
birthwt0 |>
  kable(digits = 0) |>
  kable_styling(latex_options = "hold_position")
```

## Exercises 17--18

17. Calculate an approximate 95% confidence interval for $\delta$.

```{r}
#| echo: true
bwt1 <- birthwt |> filter(smoke == 1) |> pull(bwt)
bwt0 <- birthwt |> filter(smoke == 0) |> pull(bwt)
t.test(x = bwt1, y = bwt0)$conf.int
```

A 95% CI for $\delta$ is $(-489, -79)$, in grams.

18. Interpret the output in the context of the setting.

These data provide evidence that the birth weight of babies born to mothers who
smoke during pregnancy is smaller than for babies born to mothers who do not
smoke during pregnancy. The average difference is most plausibly somewhere
between around 80 grams to around 500 grams smaller.

# Exploring sampling distributions via simulation

## Exercises 19--20

19. Explain intuitively why each of the two estimators should give a sensible
    estimate of $\mu$.

The sample mean is a standard and commonly used estimator for a population
mean. For example, we already know it is unbiased for $\mu$.

Since the population distribution is symmetric (has a symmetric pdf), the
population median coincides with the population mean, $\mu$. We expect the
sample median, $M$, to be close to the population median, thus it should be a
good estimator of the population mean as well.

20. Use simulations to show that both estimators seem to be unbiased, and
    determine which one has smaller variance. For your simulations, consider
    the case where $\mu = 3$ and $\sigma = 1$.

```{r}
# Set up the simulations.
x_mu <- 3
x_sd <- 1
n <- 100   # sample size
B <- 5000  # simulation runs

# Simulate values of the estimators.
x_bar <- numeric(B)
x_med <- numeric(B)
for (i in 1:B) {
    x <- rnorm(n, x_mu, x_sd)
    x_bar[i] <- mean(x)
    x_med[i] <- median(x)
}
```

```{r}
# Check for bias.
mean(x_bar)
mean(x_med)
```

The means of both estimators look very similar to $\mu = 3$, so the two
estimators look like they are both unbiased.

```{r}
# Calculate variances.
var(x_bar)
var(x_med)
```

We see that $\mathrm{var}(\bar{X}) < \mathrm{var}(M)$.

Given both estimators are unbiased, the better estimator is the one with
smaller variance, which is the sample mean.

## Exercises 21--23

21. Explain intuitively why each of the two estimators should give a sensible
    estimate of $\theta$.

We can derive the population mean directly from the given distribution, giving
$\mathbb{E}(X) = 5 \theta / 4$. Rearranging this equation gives

$$\theta = 4/5 \times \mathbb{E}(X)$$

and $T_1$ is obtained by replacing the expectation with the sample mean, hence
we expect $T_1$ to be a good estimator.

Similarly, we know that the expected number of observed zeros in the sample
will be $\mathbb{E}(Y) = 100 \times (1 - \theta)$. We can rearrange this to get

$$\theta = 1 - \mathbb{E}(Y) / 100$$

and replacing the expectation with the observed value gives $T_2$.

In both cases, we have calculated the expected value of the sampling
distribution of a particular statistic and related it to the parameter of
interest. Using this relationship, we derive an estimator by substituting the
statistic in place of the expected value. This strategy gives what is known as
a **method of moments** estimator. Such estimators work well in many
situations, but are not guaranteed to be optimal nor unbiased.

22. Use simulations to show that both estimators seem to be unbiased, and
    determine which one has smaller variance. For your simulations, consider
    the case where $\theta = 0.7$.

```{r}
# Set up the simulations.
theta <- 0.7
p <- c(1 - theta, 3 * theta / 4, theta / 4)
n <- 100   # sample size
B <- 5000  # simulation runs

# Simulate values of the estimators.
t1 <- numeric(B)
t2 <- numeric(B)
for (i in 1:B) {
    x <- sample(c(0, 1, 2), n, prob = p, replace = TRUE)
    t1[i] <- 0.8 * mean(x)
    t2[i] <- 1 - mean(x == 0)
}
```

```{r}
# Check for bias.
mean(t1)
mean(t2)
```

The means of both estimators look very similar to $\theta = 0.7$, so the two
estimators look like they are both unbiased.

```{r}
# Calculate variances.
var(t1)
var(t2)
```

We see that $\mathrm{var}(T_1) > \mathrm{var}(T_2)$.

23. (Challenge question, for the mathematically inclined) Prove mathematically
    that both estimators are unbiased and derive expressions for their
    variance.

First we show they are unbiased.

$$\mathbb{E}(T_1)
= (4/5) \, \mathbb{E}(\bar{X})
= (4/5) \times (5/4) \times \theta
= \theta.$$

Note that $Y \sim \mathrm{Bi}(100, 1 - \theta)$, which means that, like we
noted earlier in Exercise 21, $\mathbb{E}(Y) = 100 (1 - \theta)$. Therefore,

$$\mathbb{E}(T_2)
= 1 - (1/100) \times \mathbb{E}(Y)
= 1 - (1/100) \times 100 \, (1 - \theta)
= \theta.$$

For the variances, we need to first derive the variances of the sampling
distributions of both $\bar{X}$ and $Y$. Using the population distribution, we
can (after several lines of algebra) derive the following:

$$\mathrm{var}(X) = (7 \theta / 4) - (5 \theta / 4)^2.$$

We know that $\mathrm{var}(\bar{X}) = \mathrm{var}(X) / 100$, so we can (after
some more algebraic manipulation) show that

$$\mathrm{var}(T_1) = \frac{\theta \left(\frac{28}{25} - \theta \right)}{100}.$$

For $T_2$, we use the fact that $Y$ follows a binomial distribution (see above)
to work out the variances. We know that

$$\mathrm{var}(Y) = 100 \times \theta (1 - \theta)$$

and therefore

$$\mathrm{var}(T_2)
= (1 / 100)^2 \times 100 \times \theta (1 - \theta)
= \frac{\theta (1 - \theta)}{100}.$$

Comparing the two variances we can see that
$\mathrm{var}(T_1) \geqslant \mathrm{var}(T_2)$.

(If you can't see this, determine $\mathrm{var}(T_1) - \mathrm{var}(T_2)$.)
