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
  glab = NULL
)
```

## Arguments

- data:

  Fieldbook data frame or design object containing a fieldbook.

- factor:

  Character. Column used to color experimental units.

- fill:

  Character vector. Column or columns used as labels inside experimental
  units.

- xlab:

  Character. Optional x axis title.

- ylab:

  Character. Optional y axis title.

- glab:

  Character. Optional legend title.

## Value

A `ggplot` object.

## Details

This function works as a dispatcher. It detects the design type from the
fieldbook and sends the data to the corresponding plotting function.
