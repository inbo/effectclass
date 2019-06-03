context("is_effectclass")
test_that("is_effectclass() returns TRUE FALSE by default", {
  expect_false(is_effectclass("junk"))

  good <- classification(lcl = -2, ucl = 2, threshold = 1)
  expect_true(is_effectclass(good))

  wrong <- good
  attr(wrong, "signed") <- NULL
  expect_false(is_effectclass(wrong))

  wrong <- good
  attr(wrong, "detailed") <- NULL
  expect_false(is_effectclass(wrong))

  wrong <- good
  attr(wrong, "signed") <- c(TRUE, TRUE)
  expect_false(is_effectclass(wrong))

  wrong <- good
  attr(wrong, "detailed") <- c(TRUE, TRUE)
  expect_false(is_effectclass(wrong))

  wrong <- good
  attr(wrong, "signed") <- NA
  expect_false(is_effectclass(wrong))

  wrong <- good
  attr(wrong, "detailed") <- NA
  expect_false(is_effectclass(wrong))

  wrong <- good
  attr(wrong, "detailed") <- FALSE
  expect_false(is_effectclass(wrong))

  attr(wrong, "signed") <- FALSE
  expect_false(is_effectclass(wrong))

  attr(wrong, "detailed") <- TRUE
  expect_false(is_effectclass(wrong))

  wrong <- good
  levels(wrong) <- letters[1:10]
  expect_false(is_effectclass(wrong))

  wrong <- structure(
    factor("a"), signed = TRUE, detailed = TRUE,
    class = c("effectclass", "factor")
  )
  expect_false(is_effectclass(wrong))

  wrong <- structure(
    factor("a", levels = letters[1:4]), signed = TRUE, detailed = FALSE,
    class = c("effectclass", "factor")
  )
  expect_false(is_effectclass(wrong))

  wrong <- structure(
    factor("a", levels = letters[1:6]), signed = FALSE, detailed = TRUE,
    class = c("effectclass", "factor")
  )
  expect_false(is_effectclass(wrong))

  wrong <- structure(
    factor("a", levels = letters[1:3]), signed = FALSE, detailed = FALSE,
    class = c("effectclass", "factor")
  )
  expect_false(is_effectclass(wrong))

  wrong <- "junk"
  class(wrong) <- "effectclass"
  expect_false(is_effectclass(wrong))
})

test_that("is_effectclass() returns TRUE FALSE and a warning", {
  expect_warning(z <- is_effectclass("junk", message = "w"),
                 "'effectclass' object")
  expect_false(z)

  good <- classification(lcl = -2, ucl = 2, threshold = 1)
  expect_true(is_effectclass(good, message = "w"))

  wrong <- good
  attr(wrong, "signed") <- NULL
  expect_warning(z <- is_effectclass(wrong, message = "w"),
                 "missing the 'signed' attribute")
  expect_false(z)

  wrong <- good
  attr(wrong, "detailed") <- NULL
  expect_warning(z <- is_effectclass(wrong, message = "w"),
                 "missing the 'detailed' attribute")
  expect_false(z)

  wrong <- good
  attr(wrong, "signed") <- c(TRUE, TRUE)
  expect_warning(z <- is_effectclass(wrong, message = "w"),
                 "the 'signed' attribute must be a single TRUE or FALSE")
  expect_false(z)

  wrong <- good
  attr(wrong, "detailed") <- c(TRUE, TRUE)
  expect_warning(z <- is_effectclass(wrong, message = "w"),
                 "the 'detailed' attribute must be a single TRUE or FALSE")
  expect_false(z)

  wrong <- good
  attr(wrong, "signed") <- NA
  expect_warning(z <- is_effectclass(wrong, message = "w"),
                 "the 'signed' attribute must be a single TRUE or FALSE")
  expect_false(z)

  wrong <- good
  attr(wrong, "detailed") <- NA
  expect_warning(z <- is_effectclass(wrong, message = "w"),
                 "the 'detailed' attribute must be a single TRUE or FALSE")
  expect_false(z)

  wrong <- good
  attr(wrong, "detailed") <- FALSE
  expect_warning(z <- is_effectclass(wrong, message = "w"),
                 "a signed, coarse effectclass object requires 4 levels")
  expect_false(z)

  attr(wrong, "signed") <- FALSE
  expect_warning(z <- is_effectclass(wrong, message = "w"),
                 "an unsigned, coarse effectclass object requires 3 levels")
  expect_false(z)

  attr(wrong, "detailed") <- TRUE
  expect_warning(z <- is_effectclass(wrong, message = "w"),
                 "an unsigned, detailed effectclass object requires 6 levels")
  expect_false(z)

  wrong <- good
  levels(wrong) <- letters[1:10]
  expect_warning(z <- is_effectclass(wrong, message = "w"),
             "a signed, detailed effectclass object requires following levels:")
  expect_false(z)

  wrong <- structure(
    factor("a"), signed = TRUE, detailed = TRUE,
    class = c("effectclass", "factor")
  )
  expect_warning(z <- is_effectclass(wrong, message = "w"),
                 "a signed, detailed effectclass object requires 10 levels")
  expect_false(z)

  wrong <- structure(
    factor("a", levels = letters[1:4]), signed = TRUE, detailed = FALSE,
    class = c("effectclass", "factor")
  )
  expect_warning(z <- is_effectclass(wrong, message = "w"),
             "a signed, coarse effectclass object requires following levels:")
  expect_false(z)

  wrong <- structure(
    factor("a", levels = letters[1:6]), signed = FALSE, detailed = TRUE,
    class = c("effectclass", "factor")
  )
  expect_warning(z <- is_effectclass(wrong, message = "w"),
         "an unsigned, detailed effectclass object requires following levels:")
  expect_false(z)

  wrong <- structure(
    factor("a", levels = letters[1:3]), signed = FALSE, detailed = FALSE,
    class = c("effectclass", "factor")
  )
  expect_warning(z <- is_effectclass(wrong, message = "w"),
           "an unsigned, coarse effectclass object requires following levels:")
  expect_false(z)

  wrong <- "junk"
  class(wrong) <- "effectclass"
  expect_warning(z <- is_effectclass(wrong, message = "w"), "is not a factor")
  expect_false(z)
})

test_that("is_effectclass() returns TRUE FALSE and a warning", {
  expect_error(is_effectclass("junk", message = "e"),
               "'effectclass' object")

  good <- classification(lcl = -2, ucl = 2, threshold = 1)
  expect_true(is_effectclass(good, message = "e"))

  wrong <- good
  attr(wrong, "signed") <- NULL
  expect_error(is_effectclass(wrong, message = "e"),
               "missing the 'signed' attribute")

  wrong <- good
  attr(wrong, "detailed") <- NULL
  expect_error(is_effectclass(wrong, message = "e"),
               "missing the 'detailed' attribute")

  wrong <- good
  attr(wrong, "signed") <- c(TRUE, TRUE)
  expect_error(is_effectclass(wrong, message = "e"),
               "the 'signed' attribute must be a single TRUE or FALSE")

  wrong <- good
  attr(wrong, "detailed") <- c(TRUE, TRUE)
  expect_error(is_effectclass(wrong, message = "e"),
               "the 'detailed' attribute must be a single TRUE or FALSE")

  wrong <- good
  attr(wrong, "signed") <- NA
  expect_error(is_effectclass(wrong, message = "e"),
               "the 'signed' attribute must be a single TRUE or FALSE")

  wrong <- good
  attr(wrong, "detailed") <- NA
  expect_error(is_effectclass(wrong, message = "e"),
               "the 'detailed' attribute must be a single TRUE or FALSE")

  wrong <- good
  attr(wrong, "detailed") <- FALSE
  expect_error(is_effectclass(wrong, message = "e"),
               "a signed, coarse effectclass object requires 4 levels")

  attr(wrong, "signed") <- FALSE
  expect_error(is_effectclass(wrong, message = "e"),
               "an unsigned, coarse effectclass object requires 3 levels")

  attr(wrong, "detailed") <- TRUE
  expect_error(is_effectclass(wrong, message = "e"),
               "an unsigned, detailed effectclass object requires 6 levels")

  wrong <- good
  levels(wrong) <- letters[1:10]
  expect_error(is_effectclass(wrong, message = "e"),
             "a signed, detailed effectclass object requires following levels:")

  wrong <- structure(
    factor("a"), signed = TRUE, detailed = TRUE,
    class = c("effectclass", "factor")
  )
  expect_error(is_effectclass(wrong, message = "e"),
                 "a signed, detailed effectclass object requires 10 levels")

  wrong <- structure(
    factor("a", levels = letters[1:4]), signed = TRUE, detailed = FALSE,
    class = c("effectclass", "factor")
  )
  expect_error(is_effectclass(wrong, message = "e"),
             "a signed, coarse effectclass object requires following levels:")

  wrong <- structure(
    factor("a", levels = letters[1:6]), signed = FALSE, detailed = TRUE,
    class = c("effectclass", "factor")
  )
  expect_error(is_effectclass(wrong, message = "e"),
         "an unsigned, detailed effectclass object requires following levels:")

  wrong <- structure(
    factor("a", levels = letters[1:3]), signed = FALSE, detailed = FALSE,
    class = c("effectclass", "factor")
  )
  expect_error(is_effectclass(wrong, message = "e"),
           "an unsigned, coarse effectclass object requires following levels:")

  wrong <- "junk"
  class(wrong) <- "effectclass"
  expect_error(is_effectclass(wrong, message = "e"), "is not a factor")
})
