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

  Fieldbook data frame or design object containing a fieldbook.

- factor:

  Character scalar. Column used to color experimental units.

- fill:

  Character vector. Column or columns used as labels inside experimental
  units.

- xlab:

  Character scalar. Optional x axis title.

- ylab:

  Character scalar. Optional y axis title.

- glab:

  Character scalar. Optional legend title.

- text_size:

  Optional positive numeric scalar indicating the plot-label font size
  in typographic points (`pt`). If `NULL` or `NA`, the selected plotting
  function calculates an automatic size.

- wrap_width:

  Optional positive integer indicating the approximate maximum number of
  characters per line. If `NULL` or `NA`, labels are not wrapped.

- font_family:

  Font family used in the sketch. Defaults to `"Open Sans"`. Each
  plotting function silently falls back to `"sans"` when the requested
  font is unavailable.

- font_face:

  Font face used in the sketch. Defaults to `"plain"`, equivalent to
  regular/normal text.

## Value

A `ggplot` object.

## Details

This function works as a dispatcher. It detects the design type from the
fieldbook and sends the data to the corresponding plotting function.

The text and font arguments are forwarded to:

- [`plot_standard_design()`](https://inkaverse.com/reference/plot_standard_design.md)

- [`plot_augmented_design()`](https://inkaverse.com/reference/plot_augmented_design.md)

- [`plot_split_rcbd_design()`](https://inkaverse.com/reference/plot_split_rcbd_design.md)

The visible name `Splitplot-RCBD` and accepted spelling variants are
normalized to the stable internal identifier `"split-rcbd"` before the
plotting method is selected. The fieldbook itself is not modified.

## Examples

``` r
if (FALSE) { # \dontrun{

tarpuy_plotdesign(
  data = fieldbook,
  factor = "entry",
  fill = c("plots", "entry"),
  text_size = 9,
  wrap_width = 14,
  font_family = "Open Sans",
  font_face = "plain"
)

} # }
```
