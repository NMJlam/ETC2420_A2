---
title: "Week 4, Solutions"
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
#| echo: false
#| warning: false
#| message: false
library(tidyverse)
library(kableExtra)
library(broom)
library(ggthemes)
theme_set(theme_minimal(base_size = 12))
```

## Exercises 1--7

1.  Create a tibble named `GDS` (abbreviation for "Gender discrimination
    study") that records the data from this study, with one row for each study
    participant. That means you should have 48 rows in total.

```{r}
GDS <- tibble(gender = c(rep('male',   24), 
                         rep('female', 24)), 
              decision = c(rep('promoted',     21), 
                           rep('not promoted',  3),
                           rep('promoted',     14),
                           rep('not promoted', 10)))
```

2.  Write code that will take `GDS` and count how many of each combination of
    *gender* and *decision* were observed in the study. The output should be a
    tibble that has one row for each such combination, and a column to show the
    count for each one. (Hint: use the functions `group_by()` and `tally()`.)

```{r}
GDSS1 <- GDS |>
  group_by(gender, decision) |> 
  tally() |> 
  ungroup() 
```

3.  Write code that will restructure this into a $2 \times 2$ table. (Hint: use
    the `pivot_wider()` function.)

```{r}
GDSS2 <- GDSS1 |> 
  pivot_wider(names_from = decision, values_from = n) 
```

4.  Write further code that will create output that is similar to the following
    table. Save your tibble into a variable called `GDSS4`, which we will use
    again later on. (Hint: use the `mutate()` and `select()` functions. When
    you do so, you can use backticks to quote the value `not promoted` to
    ensure it is treated as a single value when passed as an argument to those
    functions.)

```{r}
GDSS3 <- GDSS2 |>
  mutate(total = promoted + `not promoted`) |>
  mutate(prop = round(promoted / total, digits = 3)) |>
  select(gender, promoted, `not promoted`, total, prop)

GDSS4 <- GDSS3 |>
  arrange(desc(row_number()))

GDSS4 |>
  kable() |>
  kable_styling(latex_options = "hold_position")
```

5.  Using the output from Exercise 3 (see above), write code that will turn it
    back into "long" format, so it matches the output from Exercise 2. (Hint:
    use the `pivot_longer()` function.)

```{r}
GDSS2 |> pivot_longer(-gender, names_to = "decision", values_to ="n")
```

6.  What does the table from Exercise 4 (see above) tell us? Compare the
    proportions of men and women who were promoted, and calculate the
    difference. Does it match our expectations?

The table shows that men were promoted more than women (`r GDSS4$prop[1]`
versus `r GDSS4$prop[2]`), even though the CV's in the applications were
identical. This is a difference of `r GDSS4$prop[1]- GDSS4$prop[2]`.\
Since there were equal numbers of applications for men and women, we would
expect the proportions to be about the same.

7.  Is this evidence of discrimination?

Due to randomness, we would not necessarily expect exactly the same proportions
for each group, so we don't know if this is evidence for discrimination. The
result merely suggests that there could be discrimination since this table is
descriptive only. However we can use hypothesis testing to help determine if
the difference in proportions is more likely to be due to chance, or due to
discrimination.

## Exercise 8

8.  What happens if the `facet_wrap()` function is removed from the code chunk
    shown above? Which plot do you think highlights the different proportions
    best? Is the plot now evidence for discrimination?

Without `facet_wrap()` we get a stacked bar chart.

```{r}
ggplot(GDS, aes(x=decision,color = gender, fill = gender)) + 
  geom_bar() 
```

Which one is a better visualisation is somewhat a matter of opinion. The
original version (using `facet_wrap()`) at least has the benefit that each
count is being directly compared along the same axis.

These plots are just a graphical representation of the sample table, so it is
still descriptive in nature and not yet being used for inference.

## Exercise 9

9.  Let `GDSS4` be the tibble that formed the basis for the output of Exercise
    4 (see above). The code chunk below provides three different ways of
    computing $\hat{d}$ from this tibble. While each one provides the same
    numerical value, which expression do you think should be preferred, and
    why?

```{r}
#| eval: false
d.hat <- GDSS4$prop[1] - GDSS4$prop[2]
d.hat <- GDSS4$prop[GDSS4$gender == "male"] - GDSS4$prop[GDSS4$gender == "female"]
d.hat <- (GDSS4 |> pull(prop))[1] - (GDSS4 |> pull(prop))[2]
```

Ultimately this is a matter of style and preference. However, the second
version is probably best, because it explicitly reveals that we are calculating
the positive difference $\hat{p}_M - \hat{p}_F$ and does not rely on the
ordering of the rows. What if `GDSS4` had the rows the other way around? Only
the second expression would still give the correct number. So the second is
more general (and probably easier to read).

```{r}
#| eval: true
d.hat <- GDSS4$prop[GDSS4$gender == 'male'] - GDSS4$prop[GDSS4$gender == 'female']
```

## Exercise 10

10. Report the conclusion of the hypothesis test on the basis of the above
    output, and comment on the strength of evidence against $H_0$.

The p-value for the test is 0.02566. We were not directed to use a specific
significance level, but at the conventional choice of 0.05 the test rejects
$H_0$.

Note that if we used a stricter significance level such as 0.01, then we would
not reject $H_0$.

The data show some evidence of gender discrimination against female job
candidates.

## Exercises 11--13

11. Write the missing R code.

```{r}
# Set the number of replications for test.
B <- 1000

# Set up an empty vector of length B, to be used to store the
# replicated "d.hat" values after shuffling.
Bdhat <- numeric(B)

# Initialise permutation sample with the original data.
# We will update this inside the loop with each permutation.
RGDS <- GDS

# ...comment left blank, see the exercise below...
set.seed(2025)

# Use for-loop to generate B replications of the test statistic.
# All commands inside the curly braces are repeated B times,
# with the variable r taking values in the sequence 1:B in turn.
for (r in 1:B) {

  # For iteration r, shuffle the gender variable in the data file.
  RGDS <- RGDS |> mutate(gender = sample(RGDS$gender))
  
  # Calculate the summary table for the shuffled tibble, in the
  # same way as for Exercises 2 to 4.
  RGDSS3 <- RGDS |> 
    group_by(gender, decision) |> 
    tally() |>
    ungroup() |> 
    pivot_wider(names_from = decision, values_from = n) |>
    mutate(total = `not promoted` + promoted) |> 
    mutate(prop = round(promoted / total, digits = 3))

  # Calculate the "d.hat" for the shuffled data, and
  # save it as the r-th element of the Bdhat vector.
  Bdhat[r] <- RGDSS3$prop[GDSS3$gender == "male"] -
              RGDSS3$prop[GDSS3$gender == "female"]

  # Close the for-loop with a curly brace.
}

# Proportion of Bdhat at least as big as d.hat.
# This is the approximate p-value.
pval <- sum(Bdhat >= d.hat) / B
```

12. What does the `set.seed(2025)` command do? Why is it included in the code
    for the permutation test?

The `set.seed()` function controls the value at which the random number
generator (RNG) in R is initialised. We set the random seed so we can always
reproduce the permuted draws (from the `sample()` command) and get exactly the
same p-value and test conclusions.

13. Set $B = 5000$ and re-run the code (it will take longer to run). Does the
    answer change?

Give it a go and see what happens!

## Exercises 14--17

14. Modify the above plot to add a vertical line at the observed value of
    $\hat{d}$.

```{r}
Bdhat |>
  as_tibble() |>
  mutate(r = 1:B) |>
  ggplot(aes(value)) +
  geom_histogram(fill = "blue", alpha = 0.7) +
  xlab(expression(hat(d)^"[r]")) + 
  ggtitle(expression(paste("Approximate null distribution of ",
                           hat(d) == hat(p)[M] - hat(p)[F]))) +
  geom_vline(xintercept = d.hat, color = "red")
```

15. What does this plot represent?

The plot represents a sampling distribution of proportion differences generated
under the assumption that $H_0$ is true (i.e., that there is no
discrimination). The red line shows the observed difference in proportions from
our sample.

16. Report the p-value.

The p-value from the permutation test is `r pval`.

17. Would you say that we have proved that the managers discriminate against
    women?

Although our hypothesis test can reject $H_0$ (the claim that there is no
gender discrimination) for at the conventional 5% level of significance, this
is not the same as saying $H_0$ is false, or that $H_1$ is true.

We cannot ever "prove" anything about the population with 100% certainty when
doing statistical inference. All we can do is quantify our evidence and
uncertainty about various claims or hypotheses.

It would be more helpful to ask whether we have enough evidence of
discrimination in order to take some kind of actions. How much evidence is
required will typically vary by what actions are proposed. For example,
establishing (in a criminal court) that someone is guilty of a crime typically
requires very strong evidence, whereas justifying the adoption of a new a
equity policy within an organisation may require much less evidence.
