# Experimental design in CRD and RCBD

Function to deploy field-book experiments for completely randomized
designs (CRD/DCA) and randomized complete block designs (RCBD/DBCA).

## Usage

``` r
design_repblock(
  nfactors = 1,
  factors,
  type = "crd",
  rep = 3,
  zigzag = FALSE,
  nrows = NA,
  serie = 1000,
  seed = NULL,
  project = "inkaverse",
  qrcode = "{project}{plots}"
)
```

## Arguments

- nfactors:

  Number of factors in the experiment `[numeric: 1]`.

- factors:

  Named list with the levels of each factor `[list]`.

- type:

  Type of experimental arrangement `[character: "crd", "rcbd"]`. The
  aliases `"dca"` and `"dbca"` are also accepted. The former `"lsd"`
  branch is intentionally disabled because it did not generate a valid
  Latin square design.

- rep:

  Number of replications or blocks in the experiment `[numeric: 3]`.

- zigzag:

  Arrange the physical layout in zigzag order `[logical: FALSE]`.

- nrows:

  Number of rows in the physical field layout. When missing, the number
  of replications or blocks is used.

- serie:

  Base number used to generate plot identifiers `[numeric: 1000]`.

- seed:

  Seed used for reproducible randomization `[numeric: NULL]`.

- project:

  Barcode prefix for data collection `[character: "inkaverse"]`.

- qrcode:

  Template used to concatenate QR-code fields
  `[character: "{project}{plots}"]`. The placeholder `{factors}` expands
  to all factor columns.

## Value

A list with the field-book design and parameters.

## Examples

``` r

if (FALSE) { # \dontrun{

library(inti)

factores <- list(
  "geno" = c("A", "B", "C", "D", "D", 1, NA, NULL, "NA"),
  "salt stress" = c(0, 50, 200, 200, "T0", NA, NULL, "NULL"),
  "time" = c(30, 60, 90)
)

fb <- design_repblock(
  nfactors = 3,
  factors = factores,
  type = "rcbd",
  rep = 5,
  zigzag = TRUE,
  seed = 123,
  nrows = 5,
  qrcode = "{project}{plots}"
)

dsg <- fb$fieldbook

fb %>%
  tarpuy_plotdesign(fill = "plots")

fb$parameters

} # }
```
