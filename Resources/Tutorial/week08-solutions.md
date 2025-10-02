---
title: "Week 8, Solutions"
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
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
```

# Credibility factors for Bernoulli trials

## Exercises 1--3

1. The terms are as follows:
\begin{align*}
\mathbb{E}(\theta \mid x) &= \frac{\alpha + x}{\alpha + \beta + n}  \\
\mathbb{E}(\theta)        &= \frac{\alpha}{\alpha + \beta}  \\
\hat\theta                &= \frac{x}{n}  \\
z                         &= \frac{n}{\alpha + \beta + n}  \\
\end{align*}

2.  Extending the function from last week:

    ```{r}
    beta_binomial <- function(n, x, alpha = 1, beta = 1) {
      atil <- alpha + x
      btil <- beta + n - x
      z    <- n / (alpha + beta + n)
      out  <- list(alpha_tilde = atil,
                   beta_tilde  = btil,
                   credibility_factor = z)
      return(out)
    }
    ```

3.  Define the required R function from Week 7:

    ```{r}
    beta_meanvar <- function(alpha, beta) {
      mean <- alpha / (alpha + beta)
      var  <- mean * beta / ((alpha + beta) * (alpha + beta + 1))
      out  <- list(mean = mean, var = var)
      return(out)
    }
    ```
    
    Define the required values for the statistics and hyper-parameters
    (note that to get a uniform distribution for the prior we need to
    set $\alpha = \beta = 1$):
    
    ```{r}
    n    <- 40
    xobs <- 12
    alpha <- 1
    beta  <- 1
    Q1out <- beta_binomial(n = n, x = xobs, alpha = alpha, beta = beta)
    ```

    Verify the relation:
    
    ```{r}
    # Define terms.
    mean_posterior <- beta_meanvar(Q1out$alpha_tilde, Q1out$beta_tilde)$mean
    mean_prior     <- beta_meanvar(      alpha,             beta      )$mean
    mle <- xobs / n
    z   <- Q1out$credibility_factor

    # Check the relation.
    mean_posterior == z * mle + (1 - z) * mean_prior
    ```


# A/B testing

```{r}
nA <- 1000
nB <- 1000
alphaA <-  2
betaA  <- 26
```

## Exercises 4--6

4.  The prior expected number of people who will purchase the item from Group A
    will be $\mathbb{E}[n_A \theta_A] = n_A \mathbb{E}[\theta_A]$, since $n_A$
    is fixed and known. We can use the `beta_meanvar()` function to calculate
    the prior mean $E[\theta_A]$, and then calculate the required answer, as
    follows:
    ```{r}
    priorA <- beta_meanvar(alphaA, betaA)
    nA * priorA$mean
    ```

5.  To work out the required interval, we can first calculate a corresponding
    interval from the prior on $\theta_A$ and multiply through by $n$.  In
    other words, we find values $l^{\ast}$ and $u^{\ast}$ that satisfy
    $\Pr(l^{\ast} \leqslant \theta_A \leqslant u^{\ast}) = 0.95$, and then
    define $l = n_A l^{\ast}$ and $u = n_A u^{\ast}$.  The first step is
    straightforward since we know the prior, $\theta_A \sim
    \mathrm{Beta}(\alpha = 2, \beta = 26)$.

    There are infinitely many possible intervals we could choose, but it would
    be conventional to use the *central* 95% interval, i.e. the 2.5% and 97.5%
    quantiles of the prior.

    We thus calculate the interval as follows:
    ```{r}
    LUstar <- qbeta(c(0.025, 0.975), alphaA, betaA)
    LU <- nA * LUstar
    LU
    ```

6.  The difference between the expected number of people from group A who will
    take up their promotional offer and the expected number of people from
    group B who will take up their promotional offer is zero. This is because
    $n_A = n_B$, and $\theta_A$ and $\theta_B$ are independent with the same
    (marginal) prior distribution.  Therefore, the two experiments have the
    same *a priori* expected number of 'conversions'.


## Exercise 7

7.  The posterior quantities are:

    ```{r}
    xobsA <- 63
    xobsB <- 45
    postA <- beta_binomial(n = nA, x = xobsA, alpha = alphaA, beta = betaA)
    postB <- beta_binomial(n = nB, x = xobsB, alpha = alphaA, beta = betaA)
    postA
    postB
    ```

    Thus, the posteriors for the conversion rates are:
    
    - $\mathrm{Beta}(\tilde{\alpha}_A =$ `r postA$alpha_tilde`,
         $\tilde{\beta}_A  =$ `r postA$beta_tilde`) for Group A.
    - $\mathrm{Beta}(\tilde{\alpha}_B =$ `r postB$alpha_tilde`,
          $\tilde{\beta}_B =$ `r postB$beta_tilde`) for Group B.


## Exercises 8--10

8.  The two posterior distributions are independent, because $\theta_A$ and
    $\theta_B$ are independent at the outset before we see any data, and all of
    the individual Bernoulli trials are independent too, so we can update each
    posterior without worrying about the other and the posterior distributions
    themselves are independent. This makes calculating the posterior mean and
    variance of $\Delta$ very easy.

    We have
    $$\mathbb{E}[\theta_B - \theta_A \mid x_A=63, x_B=45]
    = \mathbb{E}[\theta_B \mid x_B=45] -
      \mathbb{E}[\theta_A \mid x_A=63].$$
    Similarly, due to independence,
    $$\mathrm{var}(\theta_B - \theta_A \mid x_A=63, x_B=45)
    = \mathrm{var}(\theta_B \mid x_B=45) +
      \mathrm{var}(\theta_A \mid x_A=63).$$

    Therefore, we can easily calculate the posterior mean (and variance) of
    $\Delta$ as follows:
    ```{r}
    postA_meanvar <- beta_meanvar(postA$alpha_tilde, postA$beta_tilde)
    postB_meanvar <- beta_meanvar(postB$alpha_tilde, postB$beta_tilde)
    delta_mean <- postB_meanvar$mean - postA_meanvar$mean
    delta_var  <- postB_meanvar$var  + postA_meanvar$var
    c(mean = delta_mean, var = delta_var)
    ```

9.  Run the simulation as follows:

    ```{r}
    R <- 10000
    set.seed(24205242)

    # Simulate from the posterior.
    sampleA <- rbeta(R, postA$alpha_tilde, postA$beta_tilde)
    sampleB <- rbeta(R, postB$alpha_tilde, postB$beta_tilde)
    sampleD <- sampleB - sampleA
    bigdiff <- abs(sampleD) > 0.01

    # Visualise the posterior.
    ds <- tibble(delta = sampleD)
    ds |> ggplot(aes(x = delta, y = after_stat(density))) +
      geom_histogram(colour = "blue", fill = "blue", alpha = 0.4, bins = 100) +
      geom_density(  colour = "blue", fill = "blue", alpha = 0.4) +
      geom_vline(xintercept = c(0.01, -0.01), colour = "red") +
      ggtitle(expression(paste("Simulated posterior pdf of ", Delta))) +
      xlab(expression(Delta)) +
      theme_bw()

    # Calculate the desired quantities.
    mean(sampleD)  # approx. posterior mean
    sd(sampleD)    # approx. posterior sd
    mean(bigdiff)  # approx. prob. of a large difference
    ```

    The final lines of R code above are respectively calculating the
    following approximations:

    a) the sample mean of the draws, to approximate the posterior mean
    $$\mathbb{E}[\Delta \mid x_A=63, x_B=45]
    \approx
    \bar{\Delta} =
    \frac{1}{R} \sum_{r=1}^{R} \Delta^{(r)}$$

    b) the sample standard deviation of the draws, to approximate the posterior
    standard deviation
    $$\mathrm{sd}(\Delta \mid x_A=63, x_B=45)
    \approx
    \sqrt{\frac{1}{R - 1} \sum_{r=1}^{R} (\Delta^{(r)} - \bar\Delta)^2}$$

    c) the proportion of the $\Delta^{(r)}$ draws for which the condition
    $\lvert \Delta^{(r)} \lvert > 0.01$ holds, to approximate the corresponding
    posterior probability
    $$\Pr(\lvert \theta_B - \theta_A \lvert > 0.01 \mid x_A = 63, x_B = 45)
    \approx
    \frac{1}{R} \sum_{r=1}^{R}
    \mathrm{I}\left(\lvert \Delta^{(r)} \lvert > 0.01\right).$$

10. We have two independent samples resulting in two independent Binomial
    observations.  We could do any of the following:

    - Calculate a confidence interval for $\Delta$, using either a CLT-based
    approach (via `prop.test()`) or a bootstrap-based approach.

    - Test the hypotheses $H_0\colon \theta_A = \theta_B$ versus $H_1\colon
    \theta_A \neq \theta_B$ using a two-sample test for proportions, either
    using `prop.test()` or a permutation test.
