---
title: "Week 2, Solutions"
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
knitr::opts_chunk$set(echo    = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      echo    = TRUE,
                      eval    = TRUE)
set.seed(24205242)

library(tidyverse)
```

1.  

```{r}
dbinom(4, 8, 0.5)                        # (a)
pbinom(15, 28, 0.5)                      # (b)
1 - pbinom(12, 18, 0.5)                  # (c)
sum(dbinom(seq(1, 13, by = 2), 14, 0.5)) # (d)
```

2.  

```{r}
dpois(3, 5)      # (a)
ppois(2, 5)      # (b)
1 - ppois(3, 5)  # (c)
rpois(6, 5)      # (d)
```

3.  

```{r}
qnorm(0.75)        # (a)
qt(0.3, 10)        # (b)
qgamma(0.5, 2, 3)  # (c)
```

4.  Since this is a random sample, all of the variables are iid. Therefore they
    all have the same mean and variance.

    a)  $\mathrm{sd}(X_2) = \mathrm{sd}(X_4) = \sqrt{\mathrm{var}(X_4)} = \sqrt{4} = 2$.
    b)  By independence,
        $\mathrm{var}(X_7 + X_8) = \mathrm{var}(X_7) + \mathrm{var}(X_8) = \mathrm{var}(X_4) + \mathrm{var}(X_4) = 8$.
    c)  By the Central Limit Theorem,
        $\bar{X} \approx \mathrm{N}\left(\mathbb{E}(X_1), \frac{\mathrm{var}(X_1)}{9}\right) = \mathrm{N}\left(7, \frac{4}{9}\right)$.

5.  

```{r}
x <- rnorm(100000, 1, sqrt(2))
mean(x^2)
```

6.  

```{r}
N <- 5000
x1 <- rnorm(N)    # (a)
x2 <- rexp(N)     # (b)
x3 <- rpois(N, 1) # (c)
x4 <- rcauchy(N)  # (d)
mean(x1)
mean(x2)
mean(x3)
mean(x4)
```

The sample mean for the first three distributions is close to the population
mean. The Cauchy distribution doesn't actually have a population mean, although
it is unimodal and symmetric around 0. However, the sample mean is not close to
zero at all.

Some more insight can be gleaned by looking at how the sample mean changes as
we increase the sample size:

```{r}
ggplot() + aes(x = 1:N, y = cumsum(x1) / 1:N) + geom_line()
ggplot() + aes(x = 1:N, y = cumsum(x2) / 1:N) + geom_line()
ggplot() + aes(x = 1:N, y = cumsum(x3) / 1:N) + geom_line()
ggplot() + aes(x = 1:N, y = cumsum(x4) / 1:N) + geom_line()
```

The first 3 are converging nicely, but for the the Cauchy distribution the
sample mean jumps around erratically.

The LLN doesn't actually work for the Cauchy distribution because it doesn't
satisify one of the requirements of the law, namely that the distribution have
a finite mean.

7.  

```{r}
num_correct <- replicate(5000, sum(sample(1:20) == 1:20))
mean(num_correct)
```

8.  

```{r}
N <- 5000
part1 <- runif(N, min = 0, max = 1)
part2 <- 1 - part1
smaller_part <- ifelse(part1 < part2, part1, part2)
mean(smaller_part)
```

The `ifelse()` function is a vectorised version of an `if` statement, which we
are using here to demonstrate how it can lead to very compact code. You can
write an equivalent solution to the above using `if` instead.

9.  

This is a simple generalisation of the problem given as an example. Let's write
code that can generalise it to a streak of heads of any length.

```{r}
flips_simulation <- function(streak_length) {
  # Initialise counts.
  num_flips <- 0
  num_heads_in_row <- 0
  
  # Keep flipping coins until you get the streak of desired length.
  while (num_heads_in_row < streak_length) {
    
    # Flip a coin.
    new_flip  <- sample(c("H", "T"), size = 1)
    num_flips <- num_flips + 1
    
    # Increment the counts, depending on what you flipped.
    if (new_flip == "H") {
        num_heads_in_row <- num_heads_in_row + 1
      } else {
        num_heads_in_row <- 0
      }
  }

  return(num_flips)
}

num_flips <- replicate(5000, flips_simulation(5))
mean(num_flips)
```

10. 

```{r}
game_simulation <- function() {
  # First roll
  die_roll <- sample(1:6, size = 1)
  score <- die_roll

  # Keep rolling if you get a 6.
  while (die_roll == 6) {
    score <- score - 1  # amend previous added score to be 5 instead of 6
    die_roll <- sample(1:6, size = 1)
    score <- score + die_roll
  }

  return(score)
}

scores <- replicate(5000, game_simulation())
mean(scores)
```

11. 

First let's wrap up our previous code (from the lecture) into a function:

```{r}
birthday_prob <- function(size = 30, N = 5000) {
  mean(replicate(N, any(duplicated(sample(1:365, size = size, replace = TRUE)))))
}
```

Now we can try it out on groups of varying sizes:

```{r}
birthday_prob(20)
birthday_prob(21)
birthday_prob(22)
birthday_prob(23)
birthday_prob(24)
birthday_prob(25)
```

The answer to the problem is **23 students**. Surprisingly few!

This is a famous problem known as the [birthday
paradox](https://en.wikipedia.org/wiki/Birthday_problem).

12. 

```{r}
N <- 5000
tram  <- rgamma(N, shape =  40, rate = 4)
train <- rgamma(N, shape = 120, rate = 6)
bus   <- rgamma(N, shape =  35, rate = 5)
total <- tram + train + bus
ggplot() + aes(x = total) + geom_histogram()
mean(total)
sd(total)
mean(total > 40)
```

13. 

```{r}
sort(sample(1:500, size = 20))
```

Sorting the values in order isn't strictly necessary, but would be more
convenient to have for actual use in practice.

14. 

```{r}
student_sample <- sample(   1:5000, size = 15)
staff_sample   <- sample(9001:9200, size = 15)
combined_sample <- sort(c(student_sample, staff_sample))
combined_sample
```

15. 

    a)  Number the participants 1 to 40. The green cans can be assigned to the
        following participants:

    ```{r}
    sort(sample(1:40, size = 20))
    ```

    b)  Without randomising the allocation of cans, we won't know for sure
        whether any differences between the cans are due only to the colour or
        some other confounding variable. For example, suppose younger
        participants prefer green cans, and are also willing to pay more for
        the drink irrespective of the colour. We might mistakenly infer that
        using green cans is more profitable. Thus, without the randomisation
        the experiment is substantially less useful.
