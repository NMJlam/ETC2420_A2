---
title: "Week 7, Solutions"
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

# Food hygiene

## Exercises 1--3

1.
$\Pr(\text{fail}) = 0.8 \times 0.1 + 0.2 \times 0.7 = 0.22$,
$\Pr(\text{unsatisfactory} \mid \text{fail}) =
     \frac{0.2 \times 0.7}{0.22} = 0.636$

2.
$\Pr(\text{fail}) = 0.98 \times 0.1 + 0.02 \times 0.7 = 0.112$,
$\Pr(\text{unsatisfactory} \mid \text{fail}) =
     \frac{0.02 \times 0.7}{0.112} = 0.125$

3. *(See tree diagrams in separate PDF file.)*


# Is that a typo?  Spell checking using Bayes' Theorem

## Exercises 4--9

4. It depends a lot on the context.  In a statistics class, the word 'random'
   might be a lot more common than it would be otherwise!  We will see below
   how common they are assumed to be for this problem.

5.  The marginal probabilities for each $W_i$ are constructed by normalising the
    (historical) relative frequencies found in Table 1:
    ```{r}
    #| echo: true
    #| eval: true
    marginal <- c(0.000076, 0.00000605, 0.0000000312)
    names(marginal) <- c("random", "radon", "radom")
    ```
    The above 'marginal' probabilities are not yet normalised.  We needed to make
    sure all probabilities sum to one over the marginal distribution:
    ```{r}
    prior <- marginal / sum(marginal)
    prior
    ```

6. The values in Table 2 are likelihoods, not probabilities. So they should not
   sum to one on their own.  We only need their value up to a proportionality
   constant. When we feed them into Bayes' theorem, they will be up-or down-
   weighted according to the prior probability of each $W_i$.  So the error
   model **does not provide** a probability distribution over the values of
   $W_i$. Rather, each provides the probability of the word $y =
   \text{`radom'}$ would occur, when the word intended to be typed is really
   $W_i$. This probability is given for each $i=1,2,3$.

7.  The formulae (from Bayes' theorem) for the conditional probabilities
    $\Pr(W_1 \mid y=\text{`radom'})$,
    $\Pr(W_2 \mid y=\text{`radom'})$ and
    $\Pr(W_3 \mid y=\text{`radom'})$ are the same except for the replacement of
    the numerator:
    ```{r}
    likelihoods <- c(0.00193, 0.000143, 0.975)
    conditional <- prior * likelihoods
    conditional <- conditional / sum(conditional)
    conditional
    ```
    $\Pr(W_1 \mid y) = \frac{\Pr(y \mid W_1)\Pr(W_1)}{\Pr(y \mid W_1)\Pr(W_1) +
    \Pr(y \mid W_2)\Pr(W_2) + \Pr(y \mid W_3)\Pr(W_3)} =$ `r conditional[1]`,  
    $\Pr(W_2 \mid y) = \frac{\Pr(y \mid W_2)\Pr(W_2)}{\Pr(y \mid W_1)\Pr(W_1) +
    \Pr(y \mid W_2)\Pr(W_2) + \Pr(y \mid W_3)\Pr(W_3)} =$ `r conditional[2]`, and  
    $\Pr(W_3 \mid y) = \frac{\Pr(y \mid W_3)\Pr(W_3)}{\Pr(y \mid W_1)\Pr(W_1) +
    \Pr(y \mid W_2)\Pr(W_2) + \Pr(y \mid W_3)\Pr(W_3)}=$ `r conditional[3]`.

| $W_i$  | $\Pr(W_i \mid y = \text{`radom'})$ |
|:-------|:-----------------------------------|
| random |  `r conditional[1]`                |
| radon  |  `r conditional[2]`                |
| radom  |  `r conditional[3]`                |

: Table 3: Conditional distribution of $W_i$ given $y$.

8. The values in Table 3 are the conditional probabilities. These should sum to
   one. Considering only the three possible values of $\theta_j$, we use Bayes
   theorem to get the conditional distribution. This just involves normalising
   the product of the marginal probabilities and the corresponding likelihood
   values. It is the 'normalisation' that makes the conditional probabilities
   sum to one.

9. Actually, since the normalised probabilities $\Pr(W_i) = RF_i/c$, where $c =
   RF_1 + RF_2 + RF_3$, if we had instead used $RF_i$ instead of $\Pr(W_i)$ in
   Bayes' theorem, since $RF_i = c \times \Pr(W_i)$ then, for example,

    $$\frac{\Pr(y \mid W_1)RF_1}{\Pr(y \mid W_1)RF_1 +
                                 \Pr(y \mid W_2)RF_2 +
                                 \Pr(y \mid W_3)RF_2}
    = \frac{\Pr(y \mid W_1)c \Pr(W_1)}{\Pr(y \mid W_1)c \Pr(W_1) +
                                       \Pr(y \mid W_2)c \Pr(W_1) +
                                       \Pr(y \mid W_3)c \Pr(W_1)}$$

    Of course now we can factor out the $c$ from both the numerator and the
    denominator, and we end up back with the correct form of the conditional
    probabilities!


# Working with Bernoulli trials

## Exercises 10--17

10. The following function meets the desired specifications.

    ```{r}
    #| echo: true
    beta_binomial <- function(n, x, alpha = 1, beta = 1) {
      atil <- alpha + x
      btil <- beta + n - x
      out <- list(alpha_tilde = atil, beta_tilde = btil)
      return(out)
    }
    ```

11. We will use the format of the code chunk below together with your
    `beta_binomial()` function, to find the hyper-parameters of the posterior
    distribution, saving the result of your function evaluation as an object
    named **Q1out**. Also, to facilitate use of variables in later portions of
    the tutorial, we will add lines to the code chunk below to define each of
    the following variables:

    - **n** (the number of $\mathrm{Bernoulli}(\theta)$ trials)
    - **xobs** (the observed value of $x$)
    - **alpha** (the prior value of $\alpha$)
    - **beta** (the prior value of $\beta$)
    - **Q1out** (the output of `beta_binomial(n=n, x=xobs, alpha=alpha, beta=beta)`)

    Detail the form of the posterior distribution when the
    $\mathrm{Binomial}(n=40, \theta)$ outcome is $x=12$, and the prior
    distribution is given by $\theta \sim \mathrm{Beta}(\alpha=4,\beta=4).$
    Explain (in your own words) what this distribution represents. (Verify that
    you can get this answer by hand using the lecture notes).

    ```{r}
    #| echo: true
    n <- 40
    xobs <- 12
    alpha <- 4
    beta <- 4
    Q1out <- beta_binomial(n = n, x = xobs, alpha = alpha, beta = beta)
    ```

    When $x=$ `r xobs` is an observed outcome from a $\mathrm{Binomial}(n=$
    `r n`, $\theta)$ population distribution, and $\theta \sim
    \mathrm{Beta}(\alpha=$ `r alpha`, $\beta=$ `r beta`) is the prior
    distribution, the (updated) posterior distribution for $\theta$ is
    $\mathrm{Beta}(\tilde{\alpha}=$ `r Q1out$alpha_tilde`, $\tilde{\beta}=$
    `r Q1out$beta_tilde`).

    What we mean by this is that having observed `r xobs` successes out of
    `r n` Bernoulli trials, uncertainty about the probability of success gone
    from the prior $\mathrm{Beta}(\alpha=$ `r alpha`, $\beta=$ `r beta`)
    distribution, to the posterior $\mathrm{Beta}(\tilde{\alpha}=$
    `r Q1out$alpha_tilde`, $\tilde{\beta}=$ `r Q1out$beta_tilde`) distribution.

12. Once you have obtained your output, you can visualise them using the plot
    commands below.

    Note that the *normalised* likelihood function, given by
    $L(\theta)/\int_{0}^{1} L(\theta)~d\theta$, was plotted instead of the
    usual likelihood function, $L(\theta) = \binom{n}{x} \theta^{x} (1 -
    \theta)^{n-x}$. This was done to make the plot look nice.

    ```{r}
    #| label: firstplot
    #| echo: true
    #| eval: true
    thetaxx <- seq(0.001, 0.999, length.out = 100)
    priorxx <- dbeta(thetaxx, alpha, beta)
    postxx  <- dbeta(thetaxx, Q1out$alpha_tilde, Q1out$beta_tilde)
    likexx  <- dbinom(xobs, size = n, prob = thetaxx)
    nlikexx <- 100 * likexx / sum(likexx)

    df <- tibble(theta                   = thetaxx,
                 `prior pdf`             = priorxx,
                 `normalised likelihood` = nlikexx,
                 `posterior pdf`         = postxx)

    df_longer <- df |>
      pivot_longer(-theta, names_to = "distribution", values_to = "density")

    p1 <- df_longer |>
      ggplot(aes(x = theta, y = density,
                 colour = distribution)) +
      geom_line() +
      theme_bw()
    p1
    ```

13. Solution:
    ```{r}
    #| echo: true
    int <- c(0.2, 0.4) # define the upper and lower bounds

    # We just need to calculate probabilities from the beta distribution.
    # Remember to take the smaller probability from the larger one.

    prior_prob <- pbeta(int[2], alpha, beta) -
                  pbeta(int[1], alpha, beta)

    posterior_prob <- pbeta(int[2], Q1out$alpha_tilde, Q1out$beta_tilde) -
                      pbeta(int[1], Q1out$alpha_tilde, Q1out$beta_tilde)
    prior_prob
    posterior_prob
    ```

14. Solution:
    ```{r}
    #| echo: true
    beta_meanvar <- function(alpha, beta) {
      mean <- alpha / (alpha + beta)
      var  <- mean * beta / ((alpha + beta) * (alpha + beta + 1))
      out  <- list(mean = mean, var = var)
      return(out)
    }
    ```

15. Solution:
    ```{r}
    #| echo: true
    prior_meanvar <- beta_meanvar(alpha = 4, beta = 4)
    posterior_meanvar <- beta_meanvar(alpha = Q1out$alpha_tilde,
                                      beta  = Q1out$beta_tilde)
    prior_meanvar
    posterior_meanvar
    ```

    Under the prior distribution $\theta \sim \mathrm{Beta}(\alpha=4,
    \beta=4)$, the prior mean of $\theta$ is `r prior_meanvar$mean` and the
    prior variance of $\theta$ is `r prior_meanvar$var`. Then, after observing
    $x=$ `r xobs` out of $n=$ `r n` Bernoulli outcomes, the posterior mean for
    $\theta$ is `r posterior_meanvar$mean` and the posterior variance of
    $\theta$ is `r posterior_meanvar$var`.

16. In the denominator of Bayes' Theorem, when we integrate out the parameter
    $\theta$, what remains is only a function of $x, n, \alpha$ and $\beta$.
    Once these are all known, the integral is just a (constant) number.

17. Solution:
    ```{r}
    L <- dbinom(x = xobs, size = n, prob = 0.5)
    Pr <- dbeta(x = 0.5, shape1 = 4, shape2 = 4)
    Po <- dbeta(x = 0.5, shape1 = Q1out$alpha_tilde, shape2 = Q1out$beta_tilde)

    denom <- L * Pr / Po
    denom
    ```

    The denominator is equal to `r denom`. If we calculate this value for every
    $\theta$ in the `thetaxx` sequence constructed for the plot above, we find we
    always get the same answer too.

    ```{r}
    L <- log(dbinom(x = xobs, size = n, prob = thetaxx))
    Pr <- log(dbeta(x = thetaxx, shape1 = 4, shape2 = 4))
    Po <- log(dbeta(x = thetaxx, shape1 = Q1out$alpha_tilde, shape2 = Q1out$beta_tilde))

    denom <- exp(L + Pr - Po)
    denom
    ```
