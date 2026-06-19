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
  glab = NULL
)
```

## Arguments

- data:

  A fieldbook data frame. It must contain at least `rows` and `cols`.
  For RCBD/DBCA designs, it should also contain `block`.

- factor:

  Character. Name of the column used to color the experimental units.
  For example: `"geno"`, `"acc"`, `"nacl"`, `"block"` or `"ntreat"`. If
  `NA`, the function uses `"block"` when available; otherwise, it uses
  the third column of `data`.

- fill:

  Character vector. Name of one or more columns used as labels inside
  each experimental unit. For example: `"plots"`, `c("plots", "ntreat")`
  or `c("plots", "acc", "nacl")`. When `ntreat` is used, it is shown as
  `T1`, `T2`, etc.

- xlab:

  Character. Title for the x axis. If `NULL`, `"columns"` is used.

- ylab:

  Character. Title for the y axis. If `NULL`, `"row"` is used for
  non-RCBD designs. For RCBD/DBCA designs, the y axis is shown as
  `"blocks"`.

- glab:

  Character. Title for the legend. If `NULL`, the value of `factor` is
  used.

## Value

A `ggplot` object.

## Details

The function does not calculate the experimental design. It only plots
an existing fieldbook. Therefore, if the fieldbook was generated with
`zigzag = TRUE`, the zigzag layout is respected because the function
uses the existing `rows`, `cols` and `block` columns.

For non-blocked standard designs, such as CRD/DCA, sorted and unsorted
designs, the sketch is plotted using:

- `cols` as the x axis.

- `rows` as the y axis.

For RCBD/DBCA designs, the sketch is plotted using:

- `cols` as the x axis.

- `block` as the y axis.

In this way, each row represents one block, which makes the DBCA sketch
easier to interpret in field layout previews.

The argument `factor` controls the fill color, while `fill` controls the
text printed inside each plot. For example, `factor = "nacl"` and
`fill = c("plots", "acc", "nacl")` colors plots by NaCl level and writes
plot ID, accession and NaCl level inside each experimental unit.

## Examples

``` r
if (FALSE) { # \dontrun{

library(dplyr)
library(ggplot2)

# Example 1: sorted design without replications
factores <- list(
  geno = paste0("G", 1:12)
)

fb <- design_noreps(
  factors = factores,
  type = "sorted",
  zigzag = FALSE,
  nrows = 3,
  serie = 1000,
  seed = 123,
  project = "TEST",
  qrcode = "{project}{plots}"
)

dsg <- fb$fieldbook

plot_standard_design(
  data = dsg,
  factor = "geno",
  fill = c("plots", "ntreat")
)

# Example 2: DCA / CRD
factores_dca <- list(
  geno = paste0("G", 1:6)
)

fb_dca <- design_repblock(
  nfactors = 1,
  factors = factores_dca,
  type = "crd",
  rep = 4,
  zigzag = TRUE,
  nrows = 4,
  serie = 1000,
  seed = 123,
  project = "DCA",
  qrcode = "{project}{plots}"
)

plot_standard_design(
  data = fb_dca$fieldbook,
  factor = "geno",
  fill = c("plots", "ntreat")
)

# Example 3: DBCA / RCBD
factores_dbca <- list(
  acc = paste0("acc", 1:6),
  nacl = c(0, 100, 200, 300)
)

fb_dbca <- design_repblock(
  nfactors = 2,
  factors = factores_dbca,
  type = "rcbd",
  rep = 4,
  zigzag = TRUE,
  serie = 1000,
  seed = 123,
  project = "DBCA",
  qrcode = "{project}{plots}"
)

plot_standard_design(
  data = fb_dbca$fieldbook,
  factor = "nacl",
  fill = c("plots", "acc", "nacl"),
  glab = "NaCl"
)

} # }
```
