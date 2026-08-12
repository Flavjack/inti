# Split-plot RCBD experimental design

Generate a split-plot design under a randomized complete block design
(RCBD) structure for TARPUY.

## Usage

``` r
design_split_rcbd(
  nfactors = 2,
  factors,
  type = "split-rcbd",
  rep = 3,
  zigzag = FALSE,
  nrows = NA,
  serie = 1000,
  seed = NULL,
  project = "inkaverse",
  qrcode = "{project}{plots}{factors}"
)
```

## Arguments

- nfactors:

  Number of factors in the experiment. Splitplot-RCBD requires exactly
  two factors.

- factors:

  Named list with the factor levels. The first factor is the whole-plot
  factor and the second factor is the subplot factor.

- type:

  Design type. The canonical value is `"split-rcbd"`; accepted aliases
  are normalized by `normalize_tarpuy_design_type()`.

- rep:

  Number of replications or blocks.

- zigzag:

  Logical. If `TRUE`, plot numbering follows a continuous vertical
  serpentine path through the whole plots and blocks.

- nrows:

  Number of rows in the complete physical layout. The valid
  Splitplot-RCBD geometry is `rep * number_of_subplot_levels`; when
  missing, it is calculated automatically.

- serie:

  Base number used to generate plot identifiers. For example,
  `serie = 1000` generates plots 1001, 1002, ... in block 1 and 2001,
  2002, ... in block 2.

- seed:

  Seed used for reproducible randomization. `NA` or `NULL` leaves the
  current random-number state unchanged.

- project:

  Barcode or QR-code prefix.

- qrcode:

  Template used to concatenate QR-code fields. The placeholder
  `{factors}` expands to both experimental factors.

## Value

A list with `fieldbook` and `parameters`.

## Details

The first factor is the whole-plot factor and the second factor is the
subplot factor. Whole plots are randomized within each block and subplot
levels are randomized independently within every whole plot.

## Examples

``` r
if (FALSE) { # \dontrun{

factors <- list(
  Soil = c("S1", "S2", "S3", "S4"),
  Fertilizer = c("N1", "N2", "N3", "N4", "N5", "N6")
)

design_split_rcbd(
  factors = factors,
  rep = 3,
  zigzag = TRUE,
  seed = 123
)$fieldbook
} # }
```
