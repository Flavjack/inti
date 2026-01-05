# Field book traits

Function to export field book and traits for be used in field book app.

## Usage

``` r
tarpuy_traits(fieldbook = NULL, last_factor = NULL, traits = NULL)
```

## Arguments

- fieldbook:

  Experiment field book `[dataframe]`.

- last_factor:

  Last factor in the field book `[character: colname]`

- traits:

  Traits information `[dataframe or list]`.

## Value

list

## Details

For the traits parameters you can used shown in the Field Book app

## Examples

``` r
library(inti)

fieldbook <- inti::potato

traits <- list(
  list(variable = "altura de planta"
       , trait = "altp"
       , format = "numeric"
       , when = "30, 40, 50"
       , samples = 3
       , units = "cm"
       , details = NA
       , minimum = 0
       , maximum = 100
       )
  , list(variable = "severidad"
         , trait = "svr"
         , format = "scategorical"
         , when = "30, 40, 50"
         , samples = 1
         , units = "scale"
         , details = NA
         , categories = "1, 3, 5, 7, 9"
  )
  ,  list(variable = "foto"
          , trait = "foto"
          , format = "photo"
          , when = "hrv, pshrv"
          , samples = 1
          , units = "image"
          , details = NA
  )
  ,  list(variable = "germinacion"
          , trait = "ger"
          , format = "boolean"
          , when = "30, 40, 50"
          , samples = 1
          , units = "logical"
          , details = NA
  )
) 

fbapp <- tarpuy_traits(fieldbook, last_factor = "bloque", traits)
#> Warning: There was 1 warning in `dplyr::arrange()`.
#> ℹ In argument: `..1 = as.numeric(.data$when)`.
#> Caused by warning:
#> ! NAs introduced by coercion

if (FALSE) { # \dontrun{ 

library(inti)
library(gsheet)

url_fb <- paste0("https://docs.google.com/spreadsheets/d/"
       , "1510fOKj0g4CDEAFkrpFbr-zNMnle_Hou9O_wuf7Vdo4/edit?gid=1607116093#gid=1607116093")
       
fb <- gsheet2tbl(url_fb) 

url_ds <- paste0("https://docs.google.com/spreadsheets/d/"
       , "1510fOKj0g4CDEAFkrpFbr-zNMnle_Hou9O_wuf7Vdo4/edit?gid=1278145622#gid=1278145622")
       
ds <- gsheet2tbl(url_ds) 

fb <- ds %>% tarpuy_design()

url_trt <- paste0("https://docs.google.com/spreadsheets/d/"
       , "1510fOKj0g4CDEAFkrpFbr-zNMnle_Hou9O_wuf7Vdo4/edit?gid=1665653985#gid=1665653985")
       
traits <- gsheet2tbl(url_trt) 

fbapp <- tarpuy_traits(fb, last_factor = "cols", traits)

dsg <- fbapp[[1]]

} # }
```
