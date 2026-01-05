# Fieldbook experimental designs

Function to deploy experimental designs

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
  fbname = NA,
  qrcode = "{fbname}{plots}{factors}"
)
```

## Arguments

- data:

  Experimental design data frame with the factors and level. See
  examples.

- nfactors:

  Number of factor in the experiment(default = 1). See details.

- type:

  Type of experimental arrange `[default = "crd"]`. See details.

- rep:

  Number of replications in the experiment (default = 3).

- zigzag:

  Experiment layout in zigzag `[logic: FALSE]`.

- nrows:

  Experimental design dimension by rows `[numeric: value]`.

- serie:

  Number to start the plot id `[numeric: 100]`.

- seed:

  Replicability of draw results `[default = 0]` always random. See
  details.

- fbname:

  Barcode prefix for data collection.

- qrcode:

  String to concatenate the QR code
  `[character: {fbname}{plots}{factors}]`.

## Value

A list with the fieldbook design

## Details

The function allows to include the arguments in the sheet that have the
information of the design. You should include 2 columns in the sheet:
`{arguments}` and `{values}`. See examples. The information will be
extracted automatically and deploy the design. `nfactors` = 1: crd,
rcbd, lsd, lattice. `nfactors` = 2 (factorial): split-crd, split-rcbd
split-lsd `nfactors` \>= 2 (factorial): crd, rcbd, lsd.

## Examples

``` r
if (FALSE) { # \dontrun{

library(inti)
library(gsheet)

url <- paste0("https://docs.google.com/spreadsheets/d/"
              , "1510fOKj0g4CDEAFkrpFbr-zNMnle_Hou9O_wuf7Vdo4/edit?gid=1479851579#gid=1479851579")
# browseURL(url)

fb <- gsheet2tbl(url) 

dsg <- fb %>% tarpuy_design() 

dsg %>% 
  tarpuy_plotdesign()

} # }
```
