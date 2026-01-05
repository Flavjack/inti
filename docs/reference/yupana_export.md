# Graph options to export

Function to export the graph options and model parameters

## Usage

``` r
yupana_export(
  data,
  type = NA,
  xlab = NA,
  ylab = NA,
  glab = NA,
  ylimits = NA,
  xrotation = c(0, 0.5, 0.5),
  xtext = NA,
  gtext = NA,
  legend = "top",
  sig = NA,
  error = NA,
  color = TRUE,
  opt = NA,
  dimension = c(20, 10, 100)
)
```

## Arguments

- data:

  Result from yupana_analysis or yupana_import.

- type:

  Plot type

- xlab:

  Title for the axis x

- ylab:

  Title for the axis y

- glab:

  Title for the legend

- ylimits:

  limits of the y axis

- xrotation:

  Rotation in x axis c(angle, h, v)

- xtext:

  Text labels in x axis

- gtext:

  Text labels in group

- legend:

  the position of legends ("none", "left", "right", "bottom", "top", or
  two-element numeric vector)

- sig:

  Column with the significance

- error:

  Show the error bar ("ste" or "std").

- color:

  colored figure (TRUE), otherwise black & white (FALSE)

- opt:

  Add news layer to the plot

- dimension:

  Dimension of graphs

## Value

data frame

## Examples

``` r
if (FALSE) { # \dontrun{

library(inti)
library(gsheet)

url <- paste0("https://docs.google.com/spreadsheets/d/"
              , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/edit#gid=172957346")
# browseURL(url)

fb <- gsheet2tbl(url)

smr <- yupana_analysis(data = fb
                       , last_factor = "bloque"
                       , response = "spad_83"
                       , model_factors = "block + geno*riego"
                       , comparison = c("geno", "riego")
                       )
                       
gtab <- yupana_export(smr, type = "line", ylimits = c(0, 100, 2))

#> import

url <- paste0("https://docs.google.com/spreadsheets/d/"
              , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/edit#gid=1202800640")
# browseURL(url)

fb <- gsheet2tbl(url)

info <- yupana_import(fb)

etab <- yupana_export(info)

info2 <- yupana_import(etab)

etab2 <- yupana_export(info2)

} # }
```
