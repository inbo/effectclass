detailed_signed_palette <- c(
  `++` = "#65E060", `+` = "#A1B600", `+~` = "#B68B00", `~` = "#356196",
  `-~` = "#9C4413", `-` = "#842502", `--` = "#6D0000", `?+` = "#A6A6A6",
  `?-` = "#5A5A5A", `?` = "#808080"
)
detailed_unsigned_palette <- c(
  `**` = "#65E060", `*` = "#A1B600", `*~` = "#B68B00", `~` = "#356196",
  `?*` = "#A6A6A6", `?` = "#808080"
)
coarse_signed_palette <- c(
  `+` = "#A1B600", `~` = "#356196", `-` = "#842502", `?` = "#808080"
)
coarse_unsigned_palette <- c(`*` = "#A1B600", `~` = "#356196", `?` = "#808080")
usethis::use_data(
  detailed_signed_palette, detailed_unsigned_palette,
  coarse_signed_palette, coarse_unsigned_palette,
  internal = TRUE, overwrite = TRUE
)
