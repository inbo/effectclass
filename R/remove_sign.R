#' Remove the sign of a classifaction
#' @param classification The classification
#' @description
#' - `**` **strong effect**: `++` or `--`
#' - `*` **effect**: `+` or `-`
#' - `*~` **moderate effect**: `+~` or `-~`
#' - `~` **no effect**: `~`
#' - `?+` **potential effect**: `?+` or `?-`
#' - `?` **unknown effect**: `?`
#' @export
#' @importFrom assertthat assert_that
remove_sign <- function(classification) {
  assert_that(
    is.factor(classification),
    levels(classification) == c("++", "+", "+~", "~", "-~", "-", "--", "?+",
                                "?-", "?")
  )
  classification <- gsub("(\\+|-)", "*", as.character(classification))
  factor(
    classification,
    levels = c("**", "*", "*~", "~", "?*", "?")
  )
}
