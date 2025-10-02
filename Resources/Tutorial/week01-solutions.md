---
title: "Week 1, Solutions"
subtitle: "Statistical Thinking (ETC2420 / ETC5242)"
date-format: "[Semester 2,] YYYY"
date: "2025"
toc: true
format:
  html:
    embed-resources: true
---

```{r}
#| label: setup1
#| include: false
knitr::opts_chunk$set(echo = TRUE)
library(tinytex)
```

```{r}
#| label: load-packages
#| message: false
#| echo: true
#| eval: true
library(tidyverse)

arbuthnot <- read.csv("data/arbuthnot.csv")
```

# Exercise 1

1.  What line of R code would you use to extract the vector containing just the
    counts of girls baptised? Would it be a good idea to print it in an
    assignment?

We can use:

```{r}
arbuthnot$girls
```

Printing variables from tibbles or datasets is good to check your data, but we
would usually not want to display them fully in a report. Such printouts take
up a lot of space and are typically hard to digest in raw form. It is much
better to summarise them in a table or visualise them with an appropriate plot.

You could retain the code chunk but hide the results (how?).

# Exercise 2

2.  Is there an apparent trend in the number of girls baptized over the years?
    How would you describe it? Are there any interesting features?

There is an apparent trend in the number of girls baptized over the years, as
shown in the plot. The trend is generally upward, with the final baptism count
for girls in 1710 being nearly double that from 1629. The relationship is not
linear; there is a notably decline in counts over 1640--1650 (a period
corresponding to the English civil war) and only a minimal increase over the
final twenty years or so of the available data period, from 1690--1710.

# Exercise 3

3.  Draw a line plot of the total number of baptisms per year.

```{r}
#| eval: true
#| echo: true
#| fig-width: 5
#| fig-height: 4
arbuthnot <- mutate(arbuthnot, total = boys + girls)
ggplot(data = arbuthnot, aes(x = year, y = total)) + 
  geom_line()
```

# Exercise 4

4.  How often were there more boys born than girls?

```{r}
arbuthnot <- mutate(arbuthnot, more_boys = boys > girls)
arbuthnot$more_boys
```

There were more boys born than girls every single year!

# Exercise 5

5.  Draw a plot of the proportion of boys born over time. What do you see?

We first use the `mutate()` function to add a column to the `arbuthnot` tibble
that contains the number of boys baptised relative to the number of girls
baptised each year. Then, the plot is obtained using the `geom_line()`
function.

```{r}
#| label: plot-prop-boys-arbuthnot
arbuthnot <- mutate(arbuthnot, prop_boys = boys / (boys + girls))
ggplot(arbuthnot, aes(x = year, y = prop_boys)) + geom_line()
```

The proportion of boys baptised is a noisy series that does not appear to have
a trend. Rather, the proportion of boys looks to fluctuate somewhere around
0.52. Perhaps somewhat surprisingly, the proportion never falls below 0.5.

# Exercise 6

6.  Discuss this plot. Can it be improved?

Some things that I notice are: no title; poor y-axis scale; poor axis labels;
intrusive background shading.

# Exercise 7

7.  Use the internet or help in R to create a **TERRIBLE** graph, which has
    none of the good attributes from exercise 6.

There are no bad answers here. Get creative and have some fun!
