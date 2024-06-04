# effectclass 0.1.5

* `add_fan()` groups the traces when setting `name`.

# effectclass 0.1.4

* `add_fan()`, `add_classification()`, `reference_shape()` and
  `reference_text()` adds support for `plotly::plot_ly()`.
* `change_breaks()` generate nice breaks on a logarithmic scale.
* `format_ci()` gains a `change` argument, mostly useful in combination with 
  `percent = TRUE`.
  This converts `80%` to `-20%` and `120%` to `+20%`.
* `stat_fan()` displays by default 5% intervals.
* `stat_effect()` uses text symbols with a circle background.
* Added a `NEWS.md` file to track changes to the package.
