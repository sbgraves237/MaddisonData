
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MaddisonData

<!-- badges: start -->

[![R-CMD-check](https://github.com/sbgraves237/MaddisonData/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sbgraves237/MaddisonData/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Make it easier for humans to access data from the Maddison Data Project
in R. Later releases may include vignettes, etc., documenting analses
using the \[`KFAS`\] (Kalman filtering and smoothing, aka state space)
techniques with these data.

Objectives: Mke it relatively easy in R to do the following:

1.  Find the countries with the highest gdppc for each year for which
    data are available.

2.  Refine “1” by deleting companies with high gdppc based on something
    narroe like a commodity, e.g., oil.

3.  Plot the data available on gdppc and / or pop for a selection of
    countries, e.g., world leaders.

LATER:

4.  Build a state space / Kalman models for `gdppc` and `pop` for each
    country in the Maddison project data.

5.  Use Kalman smooth to interpolate and extrapolate (forward but not
    backwards) `gdppc` and `pop` for each country for all years that
    appear anywhere in the Maddison project data.

6.  Identify the world leader in `gdppc` for each year, refining “1”
    using KFAS interpolation.

7.  Identify the world technology leader for each year by evaluating the
    `gdppc` leader for each year and replacing any whose leadership was
    narrow like members of OPEC with a country with a broad-based
    economy like the US.

## Installation

You can install the development version of MaddisonData from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("sbgraves237/MaddisonData")
```

## Example

\[Coming soon.\]
<!--This is a basic example which shows you how to solve a common problem:-->

``` r
library(MaddisonData)
## basic example code
```
