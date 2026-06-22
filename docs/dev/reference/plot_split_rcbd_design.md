# Plot split-plot RCBD fieldbook design

Plot fieldbook sketches for split-plot designs under RCBD structure.

## Usage

``` r
plot_split_rcbd_design(
  data,
  factor = NA,
  fill = "plots",
  xlab = NULL,
  ylab = NULL,
  glab = NULL
)
```

## Arguments

- data:

  Fieldbook data frame from
  [`design_split_rcbd()`](https://inkaverse.com/dev/reference/design_split_rcbd.md).

- factor:

  Character. Column used to color experimental units.

- fill:

  Character vector. Column or columns used as labels inside each
  experimental unit.

- xlab:

  Character. Optional x axis title.

- ylab:

  Character. Optional y axis title.

- glab:

  Character. Optional legend title.

## Value

A `ggplot` object.
