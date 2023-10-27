context("stat_fan")
test_that("stat_fan works", {
  library(ggplot2)
  set.seed(20200104)
  df <- data.frame(
    a = 1:10,
    b = 4 + rnorm(10),
    c = runif(10, min = 0.1, 0.2)
  )
  p <- ggplot(df, aes(x = a, y = b, link_sd = c)) +
    stat_fan()
  expect_identical(length(p$layers), 18L)
  expect_s3_class(p$layers[[1]]$geom, "GeomRibbon")

  p <- ggplot(df, aes(x = a, y = b, link_sd = c)) +
    stat_fan(step = 0.3)
  expect_s3_class(p$layers[[1]]$geom, "GeomRibbon")
  expect_identical(length(p$layers), 3L)

  p <- ggplot(df, aes(x = a, y = b, link_sd = c)) +
    stat_fan(geom = "bar")
  expect_s3_class(p$layers[[1]]$geom, "GeomBar")
  expect_identical(length(p$layers), 36L)

  p <- ggplot(df, aes(x = a, y = b, link_sd = c)) +
    stat_fan(geom = "rect")
  expect_s3_class(p$layers[[1]]$geom, "GeomRect")
  expect_identical(length(p$layers), 18L)
})
