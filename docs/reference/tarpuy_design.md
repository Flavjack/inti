# Fieldbook experimental designs

Function to deploy the experimental designs currently supported by
TARPUY.

## Usage

``` r
tarpuy_design(
  data,
  nfactors = 1,
  type = "crd",
  rep = 2,
  zigzag = FALSE,
  nrows = NA,
  serie = 100,
  seed = NULL,
  project = NA,
  qrcode = "{project}{plots}"
)
```

## Arguments

- data:

  Experimental design data frame containing factor names and levels. A
  design sheet may also include the columns `{arguments}` and `{values}`
  to override the function arguments.

- nfactors:

  Number of factors in the experiment `[default = 1]`.

- type:

  Type of experimental arrangement `[default = "crd"]`. Supported
  designs are `"crd"`, `"rcbd"`, `"augmented"`, and `"split-rcbd"`. The
  aliases `"dca"` and `"dbca"` are accepted.

- rep:

  Number of replications or blocks in the experiment `[default = 2]`.

- zigzag:

  Arrange the physical layout in zigzag order `[logical: FALSE]`.

- nrows:

  Number of rows in the physical field layout. When missing, the
  corresponding design function calculates the layout.

- serie:

  Base number used to generate plot identifiers `[numeric: 100]`.

- seed:

  Seed used for reproducible randomization. `0`, `NA`, and `NULL`
  preserve the historical TARPUY behavior of using a random seed.

- project:

  Barcode prefix for data collection.

- qrcode:

  Template used to concatenate QR-code fields
  `[character: "{project}{plots}"]`.

## Value

A data frame containing the generated fieldbook.

## Details

The design sheet can include two optional columns named `{arguments}`
and `{values}`. Values supplied in those columns override the
corresponding function arguments. Factor columns are the remaining
columns whose names are not enclosed in braces
([`{}`](https://rdrr.io/r/base/Paren.html)) or square brackets (`[]`).

TARPUY currently dispatches only designs with an implemented and
validated generator: CRD/DCA, RCBD/DBCA, augmented, and split-plot RCBD.
Other design identifiers are rejected explicitly instead of being routed
to incomplete generators.

## Examples

``` r

if (FALSE) { # \dontrun{

library(inti)
library(gsheet)

url <- paste0(
  "https://docs.google.com/spreadsheets/d/",
  "1510fOKj0g4CDEAFkrpFbr-zNMnle_Hou9O_wuf7Vdo4/edit"
)

fb <- gsheet2tbl(url)

dsg <- fb %>% tarpuy_design()

dsg %>% tarpuy_plotdesign()

} # }
```
