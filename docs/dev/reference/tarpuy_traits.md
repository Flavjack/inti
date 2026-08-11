# Field book traits

Function to export a field book and its trait definitions for use in the
Field Book app.

## Usage

``` r
tarpuy_traits(fieldbook = NULL, last_factor = NULL, traits = NULL)
```

## Arguments

- fieldbook:

  Experimental field book `[data.frame]`.

- last_factor:

  Optional name of the last structural column to include in the CSV
  exported to Field Book `[character: colname]`.

- traits:

  Traits information `[data.frame or list]`.

## Value

A list with four elements:

- `fieldbook`: the field book including empty trait columns;

- `traits`: the trait definition table used to export the `.trt` file;

- `fb`: the base field book used to export the Field Book CSV;

- `metadata`: internal mapping between a stable Trait ID and every
  generated fieldbook column. This element is used by TARPUY and is not
  exported to the fieldbook or to Field Book mobile files.

## Details

The trait sheet can contain the columns `variable`, `{trait}`, `{when}`,
`{samples}`, `{format}`, `units`, `details`, and `categories`.

Spaces inside `{trait}`, `{when}`, and the textual part of `{samples}`
are removed. The components are joined with underscores. For example,
`{trait} = "G"`, `{when} = "Dia 1"`, and `{samples} = "plant3"` generate
`G_Dia1_plant1`, `G_Dia1_plant2`, and `G_Dia1_plant3`.

## Examples

``` r

library(inti)

fieldbook <- inti::potato

traits <- list(
  list(
    variable = "altura de planta",
    trait = "altp",
    format = "numeric",
    when = "Dia 30, Dia 40, Dia 50",
    samples = "plant3",
    units = "cm",
    details = NA,
    minimum = 0,
    maximum = 100
  ),
  list(
    variable = "severidad",
    trait = "svr",
    format = "scategorical",
    when = "30, 40, 50",
    samples = 1,
    units = "scale",
    details = NA,
    categories = "1, 3, 5, 7, 9"
  ),
  list(
    variable = "foto",
    trait = "foto",
    format = "photo",
    when = "hrv, pshrv",
    samples = NA,
    units = "image",
    details = NA
  ),
  list(
    variable = "germinacion",
    trait = "G",
    format = "boolean",
    when = "0, 1, 2",
    samples = 1,
    units = "logical",
    details = NA
  )
)

fbapp <- tarpuy_traits(fieldbook, last_factor = "bloque", traits)
#> Error in dplyr::bind_rows(traits): Can't combine `..1$samples` <character> and `..2$samples` <double>.

if (FALSE) { # \dontrun{

library(inti)
library(gsheet)

url_ds <- paste0(
  "https://docs.google.com/spreadsheets/d/",
  "1510fOKj0g4CDEAFkrpFbr-zNMnle_Hou9O_wuf7Vdo4/edit?gid=1278145622"
)

ds <- gsheet2tbl(url_ds)
fb <- ds |> tarpuy_design()

url_trt <- paste0(
  "https://docs.google.com/spreadsheets/d/",
  "1510fOKj0g4CDEAFkrpFbr-zNMnle_Hou9O_wuf7Vdo4/edit?gid=1665653985"
)

traits <- gsheet2tbl(url_trt)
fbapp <- tarpuy_traits(fb, last_factor = "cols", traits)
} # }
```
