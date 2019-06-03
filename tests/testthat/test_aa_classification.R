context("classification")
test_that("classification() works with a single threshold", {
  expect_is(
    z <- classification(lcl = -2, ucl = 2, threshold = 1),
    "effectclass"
  )
  expect_identical(as.character(z), "?")
  expect_identical(
    as.character(classification(lcl = 0.5, ucl = 2, threshold = 1)),
    "+"
  )
  expect_identical(
    as.character(classification(lcl = 1.5, ucl = 2, threshold = 1)),
    "++"
  )
  expect_identical(
    as.character(classification(lcl = 0.5, ucl = 0.9, threshold = 1)),
    "+~"
  )
  expect_identical(
    as.character(classification(lcl = -0.9, ucl = -0.5, threshold = 1)),
    "-~"
  )
  expect_identical(
    as.character(classification(lcl = -1.5, ucl = -0.5, threshold = 1)),
    "-"
  )
  expect_identical(
    as.character(classification(lcl = -2, ucl = -1.5, threshold = 1)),
    "--"
  )
  expect_identical(
    as.character(classification(lcl = -0.5, ucl = 0.5, threshold = 1)),
    "~"
  )
  expect_identical(
    as.character(classification(lcl = -1.5, ucl = 0.5, threshold = 1)),
    "?-"
  )
  expect_identical(
    as.character(classification(lcl = -0.5, ucl = 1.5, threshold = 1)),
    "?+"
  )
})
test_that("classification() works with two thresholds", {
  expect_identical(
    as.character(
      classification(lcl = -0.4, ucl = 0.4, threshold = c(1.5, -1),
                     reference = 0.5)
    ),
    "-~"
  )
})
