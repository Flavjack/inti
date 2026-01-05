# Fieldbook analysis report

Function to create a complete report of the fieldbook

## Usage

``` r
yupana_analysis(
  data,
  last_factor = NULL,
  response,
  model_factors,
  comparison,
  test_comp = "SNK",
  sig_level = 0.05,
  plot_dist = "boxplot",
  plot_diag = FALSE,
  digits = 2
)
```

## Arguments

- data:

  Field book data.

- last_factor:

  The last factor in your fieldbook.

- response:

  Response variable.

- model_factors:

  Model used for the experimental design.

- comparison:

  Factors to compare

- test_comp:

  Comprasison test c("SNK", "TUKEY", "DUNCAN")

- sig_level:

  Significal test (default: p = 0.005)

- plot_dist:

  Plot data distribution (default = "boxplot")

- plot_diag:

  Diagnostic plots for model (default = FALSE).

- digits:

  Digits number in the table exported.

## Value

list

## Examples

``` r
if (FALSE) { # \dontrun{

library(inti)

fb <- potato

rsl <- yupana_analysis(data = fb
                       , last_factor = "bloque"
                       , response = "spad_83"
                       , model_factors = "geno * treat"
                       , comparison = c("geno", "treat")
                       )

} # }
```
