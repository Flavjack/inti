# Mean comparison test

Function to compare treatment from lm or aov using data frames

## Usage

``` r
mean_comparison(
  data,
  response,
  model_factors,
  comparison,
  test_comp = "SNK",
  sig_level = 0.05
)
```

## Arguments

- data:

  Fieldbook data.

- response:

  Model used for the experimental design.

- model_factors:

  Factor in the model.

- comparison:

  Significance level for the analysis (default = 0.05).

- test_comp:

  Comparison test (default = "SNK"). Others: "TUKEY", "DUNCAN".

- sig_level:

  Significance level for the analysis (default = 0.05).

## Value

list

## Examples

``` r
if (FALSE) { # \dontrun{

library(inti)
library(gsheet)

url <- paste0("https://docs.google.com/spreadsheets/d/"
              , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/"
              , "edit#gid=172957346")
# browseURL(url)

fb <- gsheet2tbl(url)

mc <- mean_comparison(data = fb
                      , response = "spad_29"
                      , model_factors = "bloque* geno*treat"
                      , comparison = c("geno", "treat")
                      , test_comp = "SNK"
                      )
mc$comparison
mc$stat

} # }
```
