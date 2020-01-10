detailed_signed_palette <- c(
  "\u25b2", "\u25b8", "\u25d3", "\u25c9", "\u25d2", "\u25c2", "\u25bc",
  "\u25b3", "\u25bd", "\u25c7"
)
detailed_unsigned_palette <- c(
  "\u25b6", "\u25b2", "\u25d3", "\u25c9", "\u25b7", "\u25c7"
)
coarse_signed_palette <- c("\u25b2", "\u25c9", "\u25bc", "\u25c7")
coarse_unsigned_palette <- c("\u25b6", "\u25c9", "\u25c7")
usethis::use_data(
  detailed_signed_palette, detailed_unsigned_palette,
  coarse_signed_palette, coarse_unsigned_palette,
  internal = TRUE, overwrite = TRUE
)
