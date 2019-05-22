# effectclass

effectclass helps interpreting effects and visualising uncertainty. 

It classifies the effects by comparing a coverage interval with a reference, lower and upper threshold. The result is a 10 scale classification of the effect. You can reduced it to a 4 scale classification. effectclass provides `stat_effect()` and `scale_effect()` to visualise the effects as points with different shapes.

The Bank of England visualises uncertainty by using a fan plot^[Britton, E.; Fisher, P. & J. Whitley (1998). [The Inflation Report Projections: Understanding the Fan Chart](https://www.bankofengland.co.uk/-/media/boe/files/quarterly-bulletin/1998/the-inflation-report-projections-understanding-the-fan-chart). Bank of England Quarterly Bulletin. Retrieved 2019/05/22.]. Instead of displaying a single coverage interval, they recommend to display a bunch of coverage intervals with different levels of transparency.

## Installation

You can install the released version of effectclass from [GitHub](https://github.com/inbo/effectclass) with:

``` r
# installation requires the "remotes" package
# install.package("remotes")

remotes::install_github("inbo/effectclass"))
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
  geom_hline(yintercept = c(-1, 1, 0), linetype = c(3, 3, 2)) +
  geom_errorbar() +
  stat_point_change(threshold = c(-1, 1), reference = 0, size = 3) +
  scale_point_change()
```

Creating a fan plot

``` r
z <- data.frame(year = 1990:2019, dx = rnorm(30), s = rnorm(30, 1, 0.05))
z$index <- cumsum(z$dx)
library(ggplot2)
ggplot(z, aes(x = year, y = index, link_sd = s)) + stat_fan() + geom_line()
```