# effectclass

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle: stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
![GitHub](https://img.shields.io/github/license/inbo/effectclass)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html)
[![Release](https://img.shields.io/github/release/inbo/effectclass.svg)](https://github.com/inbo/effectclass/releases)
[![R build status](https://github.com/inbo/effectclass/workflows/check%20package%20on%20main/badge.svg)](https://github.com/inbo/effectclass/actions)
![r-universe name](https://inbo.r-universe.dev/badges/:name?color=c04384)
![r-universe package](https://inbo.r-universe.dev/badges/effectclass)
[![Codecov test coverage](https://codecov.io/gh/inbo/effectclass/branch/main/graph/badge.svg)](https://app.codecov.io/gh/inbo/effectclass?branch=main)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/inbo/effectclass.svg)
![GitHub repo size](https://img.shields.io/github/repo-size/inbo/effectclass.svg)
<!-- badges: end -->

The `effectclass` package helps interpreting effects and visualising uncertainty. 

It classifies the effects by comparing a coverage interval with a reference, lower and upper threshold. The result is a 10 scale classification of the effect. You can reduced it to a 4 scale classification. `effectclass` provides `stat_effect()` and `scale_effect()` to visualise the effects as points with different shapes.

The Bank of England visualises uncertainty by using a fan plot^[Britton, E.; Fisher, P. & J. Whitley (1998). [The Inflation Report Projections: Understanding the Fan Chart](https://www.bankofengland.co.uk/-/media/boe/files/quarterly-bulletin/1998/the-inflation-report-projections-understanding-the-fan-chart). Bank of England Quarterly Bulletin. Retrieved 2019-05-22.]. Instead of displaying a single coverage interval, they recommend to display a bunch of coverage intervals with different levels of transparency.

## Installation

You can install the released version of `effectclass` from [GitHub](https://github.com/inbo/effectclass) with:

``` r
# installation requires the "remotes" package
# install.package("remotes")

remotes::install_github("inbo/effectclass")
```

## Example

Classifying effect for usage in a table

``` r
library(effectclass)
z <- data.frame(
  effect = c("unknown\neffect", "potential\npositive\neffect",
             "potential\nnegative\neffect", "no effect", "positive\neffect",
             "negative\neffect", "moderate\npositive\neffect",
             "moderate\nnegative\neffect", "strong\npositive\neffect",
             "strong\nnegative\neffect"),
  estimate = c( 0,  0,    0,   0,   1,   -1,   0.5, -0.5, 1.5, -1.5),
  lcl =      c(-2, -0.9, -2,  -0.9, 0.1, -2,   0.1, -0.9, 1.1, -2),
  ucl =      c( 2,  2,    0.9, 0.9, 2,   -0.1, 0.9, -0.1, 2,   -1.1)
)
classification(z$lcl, z$ucl, threshold = c(-1, 1), reference = 0)
```

Adding a classification to a plot

``` r
library(ggplot2)
ggplot(z, aes(x = effect, y = estimate, ymin = lcl, ymax = ucl)) +
  stat_effect(threshold = c(-1, 1), reference = 0, size = 3)
```

Creating a fan plot

``` r
z <- data.frame(year = 1990:2019, dx = rnorm(30), s = rnorm(30, 1, 0.05))
z$index <- cumsum(z$dx)
library(ggplot2)
ggplot(z, aes(x = year, y = index, link_sd = s)) + stat_fan() + geom_line()
```
