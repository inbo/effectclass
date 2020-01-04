context("classification")
test_that("classification() works with a single threshold", {
  z <- classification(lcl = -2, ucl = 2, threshold = 1)
  expect_is(z, "effectclass")
  expect_identical(as.character(z), "?")
  expect_is(remove_sign(z), "effectclass")
  expect_is(remove_sign(remove_sign(z)), "effectclass")
  expect_identical(as.character(remove_sign(z)), "?")
  expect_identical(
    as.character(coarse_classification(remove_sign(z))),
    "?"
  )
  z <- coarse_classification(z)
  expect_is(z, "effectclass")
  expect_identical(as.character(z), "?")
  expect_is(remove_sign(z), "effectclass")
  expect_is(remove_sign(remove_sign(z)), "effectclass")
  expect_identical(as.character(remove_sign(z)), "?")
  z <- coarse_classification(z)
  expect_is(z, "effectclass")
  expect_identical(as.character(z), "?")

  z <- classification(lcl = 0.5, ucl = 2, threshold = 1)
  expect_identical(as.character(z), "+")
  expect_identical(as.character(remove_sign(z)), "*")
  expect_identical(
    as.character(coarse_classification(remove_sign(z))),
    "*"
  )
  z <- coarse_classification(z)
  expect_identical(as.character(z), "+")
  expect_identical(as.character(remove_sign(z)), "*")

  z <- classification(lcl = 1.5, ucl = 2, threshold = 1)
  expect_identical(as.character(z), "++")
  expect_identical(as.character(remove_sign(z)), "**")
  expect_identical(
    as.character(coarse_classification(remove_sign(z))),
    "*"
  )
  z <- coarse_classification(z)
  expect_identical(as.character(z), "+")
  expect_identical(as.character(remove_sign(z)), "*")

  z <- classification(lcl = 0.5, ucl = 0.9, threshold = 1)
  expect_identical(as.character(z), "+~")
  expect_identical(as.character(remove_sign(z)), "*~")
  expect_identical(
    as.character(coarse_classification(remove_sign(z))),
    "*"
  )
  z <- coarse_classification(z)
  expect_identical(as.character(z), "+")
  expect_identical(as.character(remove_sign(z)), "*")

  z <- classification(lcl = -0.9, ucl = -0.5, threshold = 1)
  expect_identical(as.character(z), "-~")
  expect_identical(as.character(remove_sign(z)), "*~")
  expect_identical(
    as.character(coarse_classification(remove_sign(z))),
    "*"
  )
  z <- coarse_classification(z)
  expect_identical(as.character(z), "-")
  expect_identical(as.character(remove_sign(z)), "*")

  z <- classification(lcl = -1.5, ucl = -0.5, threshold = 1)
  expect_identical(as.character(z), "-")
  expect_identical(as.character(remove_sign(z)), "*")
  expect_identical(
    as.character(coarse_classification(remove_sign(z))),
    "*"
  )
  z <- coarse_classification(z)
  expect_identical(as.character(z), "-")
  expect_identical(as.character(remove_sign(z)), "*")

  z <- classification(lcl = -2, ucl = -1.5, threshold = 1)
  expect_identical(as.character(z), "--")
  expect_identical(as.character(remove_sign(z)), "**")
  expect_identical(
    as.character(coarse_classification(remove_sign(z))),
    "*"
  )
  z <- coarse_classification(z)
  expect_identical(as.character(z), "-")
  expect_identical(as.character(remove_sign(z)), "*")

  z <- classification(lcl = -0.5, ucl = 0.5, threshold = 1)
  expect_identical(as.character(z), "~")
  expect_identical(as.character(remove_sign(z)), "~")
  expect_identical(
    as.character(coarse_classification(remove_sign(z))),
    "~"
  )
  z <- coarse_classification(z)
  expect_identical(as.character(z), "~")
  expect_identical(as.character(remove_sign(z)), "~")

  z <- classification(lcl = -1.5, ucl = 0.5, threshold = 1)
  expect_identical(as.character(z), "?-")
  expect_identical(as.character(remove_sign(z)), "?*")
  expect_identical(
    as.character(coarse_classification(remove_sign(z))),
    "?"
  )
  z <- coarse_classification(z)
  expect_identical(as.character(z), "?")
  expect_identical(as.character(remove_sign(z)), "?")

  z <- classification(lcl = -0.5, ucl = 1.5, threshold = 1)
  expect_identical(as.character(z), "?+")
  expect_identical(as.character(remove_sign(z)), "?*")
  expect_identical(
    as.character(coarse_classification(remove_sign(z))),
    "?"
  )
  z <- coarse_classification(z)
  expect_identical(as.character(z), "?")
  expect_identical(as.character(remove_sign(z)), "?")
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
