context("stat_effect")
test_that("stat_effect works", {
  library(ggplot2)
  ds <- data.frame(
    lcl = c(-5, -5, -5, -5, -2, -2, -2, 1, 1, 3),
    ucl = c(5, 2, -1, -3, 5, 2, -1, 5, 2, 5)
  )
  ds$effect <- classification(ds$lcl, ds$ucl, threshold = 2.5)
  p <- ggplot(
    ds,
    aes(y = (ucl + lcl) / 2, ymin = lcl, ymax = ucl, x = effect)
  ) +
    stat_effect(threshold = 2.5) +
    scale_effect()
  expect_identical(length(p$layers), 1L)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
})
