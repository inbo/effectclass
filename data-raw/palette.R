detailed_signed_palette <- c(
  `++` = "#65E060", `+` = "#A1B600", `+~` = "#B68B00", `~` = "#356196",
  `-~` = "#9C4413", `-` = "#842502", `--` = "#6D0000", `?+` = "grey65",
  `?-` = "grey35", `?` = "grey50"
)
detailed_unsigned_palette <- c(
  `**` = "#65E060", `*` = "#A1B600", `*~` = "#B68B00", `~` = "#356196",
  `?*` = "grey65", `?` = "grey50"
)
coarse_signed_palette <- c(
  `+` = "#A1B600", `~` = "#356196", `-` = "#842502", `?` = "grey50"
)
coarse_unsigned_palette <- c(`*` = "#A1B600", `~` = "#356196", `?` = "grey50")
usethis::use_data(
  detailed_signed_palette, detailed_unsigned_palette,
  coarse_signed_palette, coarse_unsigned_palette,
  internal = TRUE, overwrite = TRUE
)
