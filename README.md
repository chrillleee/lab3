---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# lab3

<!-- badges: start -->
  [![R-CMD-check](https://github.com/chrillleee/liu_732A94/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chrillleee/liu_732A94/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of lab3 is to ...

## Installation

You can install the development version of lab3 from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("chrillleee/liu_732A94")
```

## Example

This is a basic example which shows you how to solve a common problem:

```{r example}
library(lab3)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:

```{r cars}
summary(cars)
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

```{r pressure, echo = FALSE}
plot(pressure)
```

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.
