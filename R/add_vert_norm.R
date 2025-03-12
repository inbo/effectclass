#' Add violin style elements to a `plotly` object
#' @inheritParams add_fan
#' @export
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom plotly add_polygons
#' @family plotly add-ons
add_vert_norm <- function(
  p, x = NULL, y = NULL, ..., sd, link = c("identity", "log", "logit"), delta,
  data = NULL, inherit = TRUE, name, prob = 0.95, step = 0.01
) {
  assert_that(is.flag(inherit), noNA(inherit))
  if (inherit) {
    x <- coalesce(x, p$x$attrs[[1]][["x"]])
    y <- coalesce(y, p$x$attrs[[1]][["y"]])
    text <- coalesce(text, p$x$attrs[[1]][["text"]])
    data <- coalesce(data, p$x$visdat[[1]]())
  }
  stopifnot(
    "Please provide `x`, `y` and `data`" =
      !is.null(x) && !is.null(y) && !is.null(data)
  )
  dots <- list(...)
  if (!missing(name)) {
    dots$legendgroup <- name
  }
  sample(letters, 10, replace = TRUE) |>
    paste(collapse = "") -> hash

  dots$x <- x
  dots$y <- y
  dots$showlegend <- FALSE
  dots$inherit <- TRUE
  dots$p <- p
  dots$data <- error_vert_norm(
    data = data, x = x, y = y, sd = sd, max_prob = (1 + prob) / 2, step = step,
    hash = hash, link = link, delta = delta
  )

  do.call(add_polygons, dots)
}

#' @importFrom assertthat assert_that has_name is.number
#' @importFrom dplyr group_by
#' @importFrom stats plogis qlogis qnorm
error_vert_norm <- function(
  data, x, y, sd, max_prob = 0.95, step = 0.01, hash,
  delta, link = c("identity", "log", "logit")
) {
  if (inherits(data, "SharedData")) {
    df <- data$origData()
  } else {
    assert_that(inherits(data, "data.frame"))
    df <- data
  }
  assert_that(
    inherits(x, "formula"), inherits(y, "formula"), inherits(sd, "formula"),
    is.number(max_prob), 0.5 < max_prob, max_prob < 1,
    is.number(step), 0 < step, step < max_prob,
    has_name(df, as.character(c(x[[2]], y[[2]], sd[[2]])))
  )
  stopifnot(
    "`x` is not numeric" = is.numeric(df[[x[[2]]]]),
    "`y` is not numeric" = is.numeric(df[[y[[2]]]]),
    "`sd` is not numeric" = is.numeric(df[[sd[[2]]]])
  )
  link <- match.arg(link)

  if (missing(delta)) {
    delta <- min(diff(sort(unique(df[[x[[2]]]])))) / 2
  }

  id <- paste0("id_", hash)
  dir <- paste0("dir_", hash)
  prob <- paste0("prob_", hash)
  df[[id]] <- seq_len(nrow(df))
  expand.grid(
    id = df[[id]], prob = seq(1 - max_prob, max_prob, by = step),
    dir = c(-1, 1)
  ) |>
    rbind(
      expand.grid(id = df[[id]], prob = max_prob, dir = c(-1, 1, 2))
    ) |>
    unique() |>
    `colnames<-`(c(id, prob, dir)) |>
    merge(x = df, by = id) -> ds
  ds <- ds[order(ds[[id]], ds[[prob]] * ds[[dir]]), ]
  ds[[dir]][ds[[dir]] == 2] <- -1

  x0 <- paste0("x_", hash)
  y0 <- paste0("y_", hash)
  ds[[sd[[2]]]][ds[[sd[[2]]]] <= 0] <- NA
  ds[[y[[2]]]] <- switch(
    link, identity = ds[[y[[2]]]], log = log(ds[[y[[2]]]]),
    logit = qlogis(ds[[y[[2]]]])
  )
  ds[[y0]] <- qnorm(ds[[prob]], mean = ds[[y[[2]]]], sd = ds[[sd[[2]]]])
  ds[[x0]] <- dnorm(ds[[y0]], mean = ds[[y[[2]]]], sd = ds[[sd[[2]]]])
  ds[[x0]] <- ds[[x[[2]]]] +
    ds[[x0]] * ds[[dir]] * delta / max(ds[[x0]], na.rm = TRUE)
  ds[[x[[2]]]] <- ds[[x0]]
  ds[[x0]] <- NULL
  ds[[y[[2]]]] <- switch(
    link, identity = ds[[y0]], log = exp(ds[[y0]]), logit = plogis(ds[[y0]])
  )
  ds[[y0]] <- NULL
  ds <- group_by(ds, !!rlang::sym(id), .add = TRUE)

  if (!inherits(data, "SharedData")) {
    return(ds)
  }
  stopifnot(requireNamespace("crosstalk", quietly = TRUE))
  what_key <- data[[".__enclos_env__"]]$private$.key
  if (is.null(what_key)) {
    return(crosstalk::SharedData$new(data = ds, group = data$groupName()))
  }
  crosstalk::SharedData$new(data = ds, group = data$groupName(), key = what_key)
}
