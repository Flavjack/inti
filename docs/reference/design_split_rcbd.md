# Split-plot RCBD experimental design

Generate a split-plot design under a randomized complete block design
(RCBD) structure for Tarpuy.

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

  Number of factors in the experiment. For split-plot RCBD it must be 2.

- factors:

  List with exactly two named factors. The first factor is the
  whole-plot factor and the second factor is the subplot factor.

- type:

  Design type. Default is `"split-rcbd"`.

- rep:

  Number of replications or blocks.

- zigzag:

  Field layout in vertical zigzag order. If `TRUE`, subplot row order is
  reversed in even whole-plot columns.

- nrows:

  Experimental design dimension by rows. If `NA`, it is calculated
  automatically as `rep * number_of_subplot_levels`.

- serie:

  Number used as base for plot numbering.

- seed:

  Seed for reproducible randomization.

- project:

  Barcode or QR code prefix.

- qrcode:

  String used to concatenate QR code fields.

## Value

A list with the fieldbook design and parameters.

## Details

The first factor is interpreted as the whole-plot factor and the second
factor as the subplot factor. Factor column names are preserved in the
final fieldbook, while their experimental role is stored in
`parameters`.
