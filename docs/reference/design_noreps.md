# Experimental design without replications

Function to deploy field-book experiment without replications

## Usage

``` r
design_noreps(
  factors,
  type = "sorted",
  zigzag = FALSE,
  nrows = NA,
  serie = 1000,
  seed = NULL,
  fbname = "inkaverse",
  qrcode = "{fbname}{plots}{factors}"
)
```

## Arguments

- factors:

  Lists with names and factor vector `[list]`.

- type:

  Randomization in the list `[character: "sorted", "unsorted"]`

- zigzag:

  Experiment layout in zigzag `[logic: FALSE]`.

- nrows:

  Experimental design dimension by rows `[numeric: value]`

- serie:

  Number to start the plot id `[numeric: 1000]`.

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

factores <- list("geno" = c(1:99))

fb <- design_noreps(factors = factores
                     , type = "sorted"
                     , zigzag = F
                     , nrows = 10
                     )
                     
dsg <- fb$fieldbook

fb %>%   
  tarpuy_plotdesign(fill = "plots") 

fb$parameters

} # }
```
