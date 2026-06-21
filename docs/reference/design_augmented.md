# Experimental design: Augmented

Fieldbook generator for Augmented Designs.

## Usage

``` r
design_augmented(
  checks,
  entries,
  blocks = NULL,
  block_size = NULL,
  random = TRUE,
  zigzag = FALSE,
  dim = NA,
  serie = 1000,
  seed = NULL,
  project = "inkaverse",
  qrcode = "{project}{plots}{entry}"
)
```

## Arguments

- checks:

  Vector of check treatments.

- entries:

  Vector of new entries.

- blocks:

  Number of blocks.

- block_size:

  Number of plots per block.

- random:

  Randomize entries allocation.

- zigzag:

  Zigzag field layout.

- dim:

  Optional layout dimensions c(nrows,ncols).

- serie:

  Plot series number.

- seed:

  Random seed.

- project:

  Barcode prefix.

- qrcode:

  QR code template.

## Value

List with fieldbook and parameters.
