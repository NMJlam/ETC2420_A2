---
title: "Week 9, Solutions"
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
library(broom)
library(gridExtra)
```

# Preparing and exploring the data

## Exercises 1--3

1.  Read in the data files, into tibbles called `data2012` and `data2016`.

```{r}
#| message: false
data2016 <- read_csv("data/rio_olympics2016.csv")
data2012 <- read_csv("data/london_olympics2012.csv")
```

2.  Create bar plots of the total medal count for both datasets (2012 and
    2016).

```{r}
#| fig-height: 3.5
p1a <- data2016 |>
  ggplot(aes(x = Total)) +
  geom_bar(fill = "blue", colour = "blue") +
  theme_bw() +
  ggtitle("Team total medal tally from\n2016 Rio Olympics",
          "Teams that won at least one medal") +
  ylab("Number of teams") +
  xlab("Medals per team")

p1b <- data2012 |>
  ggplot(aes(x = Total)) +
  geom_bar(fill = "blue", colour = "blue") +
  theme_bw() +
  ggtitle("Team total medal tally from\n2012 London Olympics",
          "Teams that won at least one medal") +
  ylab("Number of teams") +
  xlab("Medals per team")

grid.arrange(p1a, p1b, ncol = 2)
```

3.  Describe the distribution of the total medal counts. What is the main
    noticeable feature?

Both distributions are heavily right-skewed, with a large number of teams with
few medals and a small number with a much larger number of medals.

## Exercise 4

```{r}
data2012_prejoin <- data2012 |>
  select(Country, Total) |>
  rename(Total_2012 = Total)

data2016_prejoin <- data2016 |>
  select(Country, Total) |>
  rename(Total_2016 = Total)

oly <- full_join(data2012_prejoin,
                 data2016_prejoin,
                 by = "Country")
```

4.  Can you spot any problems with the `oly` tibble produced using
    `full_join()`?

```{r}
head(oly)
tail(oly)
```

Some countries have missing values (those that only appear in one of the
original tibbles).

## Exercises 5--6

```{r}
oly <- inner_join(data2012_prejoin,
                  data2016_prejoin,
                  by = "Country")
```

5.  Draw a scatter plot of the total medal counts, comparing 2016 with 2012.

```{r}
oly |>
  ggplot(aes(x = Total_2012, y = Total_2016)) +
  geom_point() +
  theme_bw()
```

6.  Describe the relationship you can see on the scatter plot.

The data look like they have a strong positive linear relationship.

# Fitting a simple linear regression

## Exercise 7

7.  Use the `lm()` function (as demonstrated in the lectures) to fit the above
    model. Store your result in a variable called `oly_lm`.

```{r}
oly_lm <- lm(Total_2016 ~ Total_2012, data = oly)
```

## Exercise 8

```{r}
coefs <- tidy(oly_lm)
```

8.  Draw the scatter plot again, this time adding the fitted line.

```{r}
oly |>
  ggplot(aes(x = Total_2012, y = Total_2016)) +
  geom_point() +
  geom_abline(intercept = coefs$estimate[1],
              slope     = coefs$estimate[2],
              colour    = "blue") +
  theme_bw()
```

**Tip:** you can use `geom_smooth()` to add the ordinary least squares (OLS)
regression line to any scatter plot without needing to actually fit the model:

```{r}
#| eval: false
oly |>
  ggplot(aes(x = Total_2012, y = Total_2016)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "blue") +
  theme_bw()
```

## Exercise 9

```{r}
#| fig-height: 5
#| fig-width: 5
plot(oly_lm, 1:2)
```

9.  Describe the distribution of the residuals. Do you notice any patterns or
    distinctive features?

It looks like there is increased variance (width of dots from the horizontal
line) as the number of medals (in 2012) increases. This is known as
*heteroscedasticity*, and commonly occurs with data that increase in size.

The QQ plot highlights a few very large residuals, which we can see are from
the few countries with very large medal counts.

## Exercises 10--11

10. Draw your own version of the QQ plot of the residuals (versus the
    corresponding theoretical normal quantiles), and also a histogram.

```{r}
#| fig-height: 3.5
#| fig-width: 7
resids <- residuals(oly_lm)

rhis <- oly |>
  ggplot(aes(x = resids, y = after_stat(density))) + 
  geom_histogram(bins   = 20,
                 colour = "steelblue",
                 fill   = "steelblue",
                 alpha  = 0.2) +
  ggtitle("Residual histogram") +
  xlab("Residuals") +
  ylab("Density") +
  theme_bw()

rqq <- oly |>
  ggplot(aes(sample = resids)) +
  stat_qq(distribution = qnorm) +
  stat_qq_line(distribution = qnorm, color = "red") +
  theme(aspect.ratio = 1) + 
  ggtitle("Residual QQ plot") +
  xlab("Theoretical quantiles") +
  ylab("Residuals") +
  theme_bw()

grid.arrange(rhis, rqq, ncol = 2)
```

11. For a good fit, the distribution of residuals should be approximately
    symmetric and bell-shaped. Is that what you can see?

The residual plot looks symmetric, but has "fat" tails. This is confirmed in
the QQ plot.

## Exercises 12--17

12. Create a new tibble that is the same as `oly` but with the USA excluded.

```{r}
oly_noUSA <- oly |> filter(Country != "UnitedStates")
```

13. Fit the regression model to the new tibble. How do the regression
    coefficients compare to the original model?

```{r}
oly_noUSA_lm <- lm(Total_2016 ~ Total_2012, data = oly_noUSA)
coefs_noUSA <- tidy(oly_noUSA_lm)
coefs_noUSA
```

The slope has reduced by about 0.1, which is a large amount (compared to its
standard error). The intercept has increase by 1 unit, which is less notable
(it has a much higher standard error).

14. What is the $R^2$ of this new model?

```{r}
glance(oly_noUSA_lm)$r.squared
```

This is similar to the previous model.

15. Draw diagnostic plots for this new model.

```{r}
#| fig-height: 5
#| fig-width: 5
plot(oly_noUSA_lm, 1:2)
```

16. Draw a scatter plot of the data, overlayed with the fitted lines from both
    models.

```{r}
oly |>
  ggplot(aes(x = Total_2012, y = Total_2016)) +
  geom_point() +
  geom_abline(intercept = coefs$estimate[1],
              slope     = coefs$estimate[2],
              colour    = "blue") +
  geom_abline(intercept = coefs_noUSA$estimate[1],
              slope     = coefs_noUSA$estimate[2],
              colour    = "red") +
  theme_bw()
```

17. What can you conclude about the models?

We can see that the USA has an influence on the slope. It has decreased by
around 0.1 in magnitude.

The overall fit looks similar, with an $R^2$ still around 93% and residuals
having a similar pattern to before, albeit with one fewer extreme point.

# Measures of influence (Leverage and Cook's distance)

```{r}
aug_oly <- augment(oly_lm) |> mutate(Country = oly$Country)
```

## Exercise 18

18. Find all high-leverage data points from the original fitted model, using
    this rule of thumb.

```{r}
p <- 2
n <- nrow(oly)
threshold <- 2 * p / n
threshold
```

The cutoff for high leverage is $2p / n$, where here we have $p = 2$ and
$n = 73$. Thus, we want to lookout for any `.hat` values that are much greater
than `r threshold`.

```{r}
aug_oly |>
  arrange(desc(.hat)) |>
  select(Country, .hat) |>
  filter(.hat > threshold)
```

It may be easier to visualise this with a histogram of the leverage scores, and
a vertical red line at the threshold value:

```{r}
#| fig-height: 3.5
aug_oly |>
  ggplot(aes(x = .hat)) +
  geom_bar(colour = "blue", fill = "blue") +
  geom_vline(xintercept = threshold, colour = "red", linewidth = 1)  +
  theme_bw()
```

## Exercise 19

19. Find all countries with a high Cook's $D$ value, using the rule of thumb.

```{r}
aug_oly |>
  arrange(desc(.cooksd)) |>
  dplyr::select(Country, .cooksd) |>
  filter(.cooksd >= threshold)
```

```{r}
aug_oly |>
  ggplot(aes(x = .cooksd)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 30) +
  geom_vline(xintercept = threshold, colour = "red", linewidth = 1) +
  theme_bw()
```

Again, 4 countries exceed the threshold. However, only the top 3 have clearly
very high influence, with Great Britain have a value that's much closer to the
rest of the data.

# Improving the model

Transforming the data:

```{r}
oly_trans <- oly |>
  mutate(log2012  = log(Total_2012),
         log2016  = log(Total_2016),
         sqrt2012 = sqrt(Total_2012),
         sqrt2016 = sqrt(Total_2016))
```

## Exercises 20--22

20. Repeat the modelling using the log-transformed counts. Has it improved the
    fit of the model?

```{r}
#| fig-height: 4
#| fig-width: 4
oly_log_lm <- lm(log2016 ~ log2012, oly_trans)
plot(oly_log_lm, c(1, 2, 5))
```

By moving to a log scale, we have eliminated the points of high influence.
However, we still have some heteroscedasticity, now for the points on the other
end of the scale (those with low medal counts). So, it looks like this
transformation is too "strong", it has dampened down the variation for the
large counts too much.

21. Repeat the modelling using the square-root-transformed counts. Has it
    improved the fit of the model?

```{r}
#| fig-height: 4
#| fig-width: 4
oly_sqrt_lm <- lm(sqrt2016 ~ sqrt2012, oly_trans)
plot(oly_sqrt_lm, c(1, 2, 5))
```

This transformation looks like a happy compromise between the original scale
and the log scale. We have a more consistent amount of variation in the
residuals across the whole range, and the previously highly influential points
still have low Cook's $D$.

This looks like the best model of the three we have tried.

22. Draw a scatter plot of the data, and overlay the three different models
    that we have fitted to it (not including the one where we have removed
    USA).

```{r}
# The fitted model with the log transformation.
fitted_log <- function(x) {
  coefs_log <- tidy(oly_log_lm)
  y <- exp(coefs_log$estimate[1] + coefs_log$estimate[2] * log(x))
  return(y)
}

# The fitted model with the square root transformation.
fitted_sqrt <- function(x) {
  coefs_sqrt <- tidy(oly_sqrt_lm)
  y <- (coefs_sqrt$estimate[1] + coefs_sqrt$estimate[2] * sqrt(x))^2
  return(y)
}

ggplot(oly) +
  aes(x = Total_2012, y = Total_2016) +
  geom_point() +
  geom_abline(intercept = coefs$estimate[1],
              slope     = coefs$estimate[2],
              colour    = "blue") +
  geom_function(fun = fitted_log,  colour = "green") +
  geom_function(fun = fitted_sqrt, colour = "orange") +
  theme_bw()
```

The square-root-transformed model fit is actually quite close to the original
fit. However, it will have give different confidence and prediction intervals
(which are not shown here).
