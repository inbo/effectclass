context("format")
test_that("format works as expected", {
  z <- classification(lcl = -2, ucl = 2, threshold = 1)
  expect_identical(format(z, type = "markdown"), "`?`")
  expect_identical(format(z, type = "plot"), enc2utf8("\u25c7"))
  z <- remove_sign(z)
  expect_identical(format(z, type = "markdown"), "`?`")
  expect_identical(format(z, type = "plot"), enc2utf8("\u25c7"))

  z <- classification(lcl = -0.5, ucl = 0.5, threshold = 1)
  expect_identical(format(z, type = "markdown"), "`~`")
  expect_identical(format(z, type = "plot"), enc2utf8("\u25c9"))
  z <- remove_sign(z)
  expect_identical(format(z, type = "markdown"), "`~`")
  expect_identical(format(z, type = "plot"), enc2utf8("\u25c9"))

  z <- classification(lcl = 0.5, ucl = 1.5, threshold = 1)
  expect_identical(format(z, type = "markdown"), "`+`")
  expect_identical(format(z, type = "plot"), enc2utf8("\u25e5"))
  z <- remove_sign(z)
  expect_identical(format(z, type = "markdown"), "`*`")
  expect_identical(format(z, type = "plot"), enc2utf8("\u25e5"))

  z <- classification(lcl = -1.5, ucl = -0.5, threshold = 1)
  expect_identical(format(z, type = "markdown"), "`-`")
  expect_identical(format(z, type = "plot"), enc2utf8("\u25e2"))
  z <- remove_sign(z)
  expect_identical(format(z, type = "markdown"), "`*`")
  expect_identical(format(z, type = "plot"), enc2utf8("\u25e5"))
})

test_that("print works as expected", {
  z <- classification(lcl = c(-2, -0.5), ucl = c(-0.5, 0.5), threshold = 1)
  expect_output(print(z), "^- ~$")
  expect_output(print(z, type = "markdown"), "^`-` `~`$")
  expect_output(print(z, sep = "_"), "^-_~$")
})

test_that("selection works", {
  z <- classification(lcl = c(-2, -0.5), ucl = c(2, 0.5), threshold = 1)
  y <- classification(lcl = -0.5, ucl = 0.5, threshold = 1)
  expect_identical(z[2], y)
})

test_that("unlist works", {
  expect_identical(unlist("junk"), "junk")
  z <- classification(lcl = c(-2, -0.5), ucl = c(2, 0.5), threshold = 1)
  expect_identical(unlist(z), z)
})
