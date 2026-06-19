# Split-plot experimental designs

Dispatch split-plot experimental designs.

## Usage

``` r
design_split(
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

  Number of factors in the experiment.

- factors:

  List with factor levels.

- type:

  Split-plot design type.

- rep:

  Number of replications.

- zigzag:

  Field layout in zigzag.

- nrows:

  Experimental design dimension by rows.

- serie:

  Number to start the plot id.

- seed:

  Seed for randomization.

- project:

  Barcode prefix.

- qrcode:

  String to concatenate the QR code.

## Value

A list with the fieldbook design and parameters.
