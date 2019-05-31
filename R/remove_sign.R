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
  is_effectclass(classification, message = "error")
  if (!attr(classification, "signed")) {
    return(classification)
  }
  new_levels <- gsub("(\\+|-)", "*", as.character(classification))
  if (attr(classification, "detailed")) {
    structure(
      factor(
        new_levels,
        levels = c("**", "*", "*~", "~", "?*", "?")
      ),
      signed = FALSE,
      detailed = TRUE,
      class = c("effectclass", "factor")
    )
  } else {
    structure(
      factor(
        new_levels,
        levels = c("*", "~", "?")
      ),
      signed = FALSE,
      detailed = FALSE,
      class = c("effectclass", "factor")
    )
  }
}
