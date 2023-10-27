context("format_ci")
test_that("format_ci()", {
  expect_identical(
    format_ci(0.512345, 1),
    "0.5 (-1.4; 2.5)"
  )
  expect_identical(
    format_ci(0.512345, lcl = -1.42, ucl = 2.51),
    "0.5 (-1.4; 2.5)"
  )
  expect_identical(
    format_ci(0.512345, 1, link = "log"),
    "1.7 (0.2; 11.8)"
  )
  expect_identical(
    format_ci(0.512345, 1, link = "logit"),
    "0.63 (0.19; 0.92)"
  )
  expect_identical(
    format_ci(0.512345, 1, sign = TRUE),
    "+0.5 (-1.4; +2.5)"
  )
  expect_identical(
    format_ci(0.512345, 1, link = "logit", percent = TRUE),
    "63% (19%; 92%)"
  )
  expect_identical(format_ci(0, lcl = 0, ucl = 0), "0 (0; 0)")
  expect_identical(format_ci(1, lcl = 1, ucl = 1), "1 (1; 1)")
  expect_identical(
    format_ci(0:1, lcl = c(0, 0), ucl = c(0, 2)),
    c("0 (0; 0)", "1.0 (0.0; 2.0)")
  )
})
