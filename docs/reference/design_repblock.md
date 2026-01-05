# Experimental design in CRD and RCBD

Function to deploy field-book experiment for CRD and RCBD

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
  fbname = "inkaverse",
  qrcode = "{fbname}{plots}{factors}"
)
```

## Arguments

- nfactors:

  Number of factor in the experiment `[numeric: 1]`.

- factors:

  Lists with names and factor vector `[list]`.

- type:

  Type of experimental arrange `[character: "crd" "rcbd" "lsd"]`

- rep:

  Number of replications in the experiment `[numeric: 3]`.

- zigzag:

  Experiment layout in zigzag `[logic: FALSE]`.

- nrows:

  Experimental design dimension by rows `[numeric: value]`

- serie:

  Number to start the plot id `[numeric: 100]`.

- seed:

  Replicability from randomization `[numeric: NULL]`.

- fbname:

  Bar code prefix for data collection `[character: "inkaverse"]`.

- qrcode:

  Concatenate the QR code `[character: "{fbname}{plots}{factors}"]`

## Value

A list with the field-book design and parameters

## Examples

``` r
if (FALSE) { # \dontrun{

library(inti)

factores <- list("geno" = c("A", "B", "C", "D", "D", 1, NA, NA, NULL, "NA")
                 , "salt stress" = c(0, 50, 200, 200, "T0", NA, NULL, "NULL")
                 , time = c(30, 60, 90)
                 )

fb <-design_repblock(nfactors = 2
                     , factors = factores
                     , type = "rcbd"
                     , rep = 5
                     , zigzag = T
                     , seed = 0
                     , nrows = 20
                     , qrcode = "{fbname}{plots}{factors}"
                     )
                     
dsg <- fb$fieldbook

fb %>%   
  tarpuy_plotdesign(fill = "plots") 

fb$parameters

} # }
```
