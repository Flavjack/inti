# Fieldbook plot experimental designs

Plot fieldbook sketches according to the experimental design type.

## Usage

``` r
tarpuy_plotdesign(
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

  Fieldbook data frame or a design object containing a fieldbook.

- factor:

  Character scalar. Column used to color experimental units. When
  omitted, `NA`, an empty string or `"auto"`, TARPUY selects a design-
  appropriate default: the first experimental factor for CRD, RCBD and
  Splitplot-RCBD, and `type` for augmented designs.

- fill:

  Character vector. Column or columns used as labels inside the
  experimental units. Defaults to `"plots"`.

- xlab:

  Character scalar. Optional x-axis title. When `NULL`, each plotter
  determines the title from the design geometry.

- ylab:

  Character scalar. Optional y-axis title. When `NULL`, each plotter
  determines whether the physical `rows` represent rows, blocks or
  subplot positions.

- glab:

  Character scalar. Optional legend title.

- text_size:

  Optional positive numeric scalar indicating the plot-label font size
  in typographic points (`pt`). When `NULL` or `NA`, each plotter
  calculates a suitable size automatically.

- wrap_width:

  Optional positive integer retained for backward compatibility. When
  `NULL` or `NA`, label wrapping is calculated automatically by the
  selected plotter. This argument does not need to be exposed as a
  control in the TARPUY interface.

- font_family:

  Character scalar retained for programmatic use. Defaults to
  `"Open Sans"`; the plotters fall back to `"sans"` when necessary.

- font_face:

  Character scalar retained for programmatic use. One of `"plain"`,
  `"bold"`, `"italic"` or `"bold.italic"`.

## Value

A `ggplot` object.

## Details

This function is the common plotting interface used by TARPUY. It reads
the design stored in the fieldbook, chooses the corresponding plotter
and sends the same plotting arguments to all supported design types.

Supported designs and plotters are:

- CRD and RCBD:
  [`plot_standard_design()`](https://inkaverse.com/reference/plot_standard_design.md).

- Augmented:
  [`plot_augmented_design()`](https://inkaverse.com/reference/plot_augmented_design.md).

- Splitplot-RCBD:
  [`plot_split_rcbd_design()`](https://inkaverse.com/reference/plot_split_rcbd_design.md).

The function never recalculates or rearranges the design. All plotters
use `cols` as the x coordinate and `rows` as the y coordinate. Columns
such as `block` are used only for labels, grouping or faceting.

`wrap_width`, `font_family` and `font_face` remain available to avoid
breaking existing programmatic calls, but the TARPUY frontend should
expose only the general `text_size` control. Automatic label wrapping is
used when `wrap_width = NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{

tarpuy_plotdesign(
  data = fieldbook,
  factor = "auto",
  fill = c("plots", "ntreat"),
  text_size = 9
)

} # }
```
