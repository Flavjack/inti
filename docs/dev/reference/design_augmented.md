# Experimental design: Augmented

Fieldbook generator for Augmented Designs.

## Usage

``` r
design_augmented(
  checks,
  entries,
  blocks = NULL,
  eu_block = NULL,
  random = TRUE,
  zigzag = FALSE,
  dim = NA,
  serie = 1000,
  seed = NULL,
  project = "inkaverse",
  qrcode = "{project}{plots}{entry}",
  separate_checks = TRUE
)
```

## Arguments

- checks:

  Vector of check treatments.

- entries:

  Vector of new entries.

- blocks:

  Optional number of blocks. If `NULL`, it is calculated from `entries`,
  `checks` and `eu_block`.

- eu_block:

  Number of experimental units per block.

- random:

  Randomize entries allocation and positions inside each block.

- zigzag:

  Zigzag field layout.

- dim:

  Optional layout dimensions c(nrows, ncols).

- serie:

  Plot series number.

- seed:

  Random seed. `0` or `NULL` means no fixed seed.

- project:

  Barcode prefix.

- qrcode:

  QR code column template.

- separate_checks:

  Logical. When possible, prevent adjacent checks inside each block
  using constrained randomization.

## Value

List with fieldbook and parameters.
