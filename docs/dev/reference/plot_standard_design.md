# Plot standard fieldbook experimental designs

Plot standard fieldbook sketches for simple experimental designs
generated in Tarpuy. This function is intended for designs with a
regular fieldbook layout, such as completely randomized designs,
randomized complete block designs, sorted designs and unsorted designs.

## Usage

``` r
plot_standard_design(
  data,
  factor = NA,
  fill = "plots",
  xlab = NULL,
  ylab = NULL,
  glab = NULL,
  text_size = NULL,
  wrap_width = NULL,
  font_family = "Open Sans",
  font_face = "plain"
)
```

## Arguments

- data:

  A fieldbook data frame. It must contain at least `rows` and `cols`.
  For RCBD/DBCA designs, it should also contain `block`.

- factor:

  Character scalar. Name of the column used to color the experimental
  units. If missing, `"block"` is used when available; otherwise, the
  third column of `data` is used.

- fill:

  Character vector. Names of one or more columns used as labels inside
  each experimental unit. When `ntreat` is used, it is displayed as
  `T1`, `T2`, etc.

- xlab:

  Character scalar. Title for the x axis. If `NULL`, `"columns"` is
  used.

- ylab:

  Character scalar. Title for the y axis. If `NULL`, `"row"` is used for
  non-RCBD designs. For RCBD/DBCA designs, `"blocks"` is used.

- glab:

  Character scalar. Legend title. If `NULL`, `factor` is used.

- text_size:

  Optional positive numeric scalar. Text size passed to
  [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html).
  If `NULL` or `NA`, it is calculated automatically from the number of
  columns selected in `fill`.

- wrap_width:

  Optional positive integer. Maximum approximate number of characters
  per line in plot labels. If `NULL` or `NA`, labels are not wrapped.
  Underscores are shown as spaces only in the plotted label; the
  original fieldbook values are not modified.

- font_family:

  Character scalar. Font family used by the sketch. Defaults to
  `"Open Sans"`. If the font cannot be found through `systemfonts`,
  `"sans"` is used as a fallback.

- font_face:

  Character scalar. Font face used in labels, axes and legends. Defaults
  to `"plain"` (Open Sans Regular).

## Value

A `ggplot` object.

## Details

The function does not calculate the experimental design. It only plots
an existing fieldbook. Therefore, if the fieldbook was generated with
`zigzag = TRUE`, the zigzag layout is respected because the function
uses the existing layout columns.

Non-blocked standard designs are plotted using `cols` on the x axis and
`rows` on the y axis. RCBD/DBCA designs retain their existing Tarpuy
representation: `cols` on the x axis and `block` on the y axis.

Label formatting affects only the sketch. It does not change `entry`,
`ntreat`, QR codes or any other fieldbook value.

## Examples

``` r
if (FALSE) { # \dontrun{

plot_standard_design(
  data = fieldbook,
  factor = "geno",
  fill = c("plots", "entry"),
  text_size = 2.5,
  wrap_width = 14,
  font_family = "Open Sans",
  font_face = "plain"
)

} # }
```
