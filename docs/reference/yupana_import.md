# Import information from data summary

Graph summary data

## Usage

``` r
yupana_import(data)
```

## Arguments

- data:

  Summary information with options

## Value

list

## Examples

``` r
if (FALSE) { # \dontrun{

library(inti)
library(gsheet)

url <- paste0("https://docs.google.com/spreadsheets/d/"
              , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/edit?gid=2137596914#gid=2137596914")
# browseURL(url)

fb <- gsheet2tbl(url)

info <- yupana_import(fb)

} # }
```
