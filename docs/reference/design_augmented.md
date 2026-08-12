# Experimental design: Augmented

Fieldbook generator for augmented experimental designs. Every check
occurs once in each block and every test entry occurs once in the
complete design.

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

  Vector of new or test entries.

- blocks:

  Optional number of blocks. If `NULL`, it is calculated from `entries`,
  `checks` and `eu_block`.

- eu_block:

  Number of experimental units per block. It must be greater than the
  number of checks.

- random:

  Logical. Randomize test-entry allocation and positions inside each
  block.

- zigzag:

  Logical. Arrange the physical field layout in zigzag order.

- dim:

  Optional physical layout dimensions `c(nrows, ncols)`. The product
  must equal the total number of experimental units.

- serie:

  Base number used to generate plot identifiers.

- seed:

  Random seed. `0`, `NA` or `NULL` means that no fixed seed is set
  inside this function. TARPUY stores an effective seed in the design
  sheet before calling the design generator.

- project:

  Barcode prefix.

- qrcode:

  QR-code column template. The default is `"{project}{plots}{entry}"`.

- separate_checks:

  Logical. When enough positions are available, place checks in
  non-adjacent positions inside each block. With `random = FALSE`, this
  placement is deterministic.

## Value

A list with `fieldbook` and `parameters`.
