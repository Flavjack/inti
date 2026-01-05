# Multivariate Analysis

Multivariate analysis for PCA and HCPC

## Usage

``` r
yupana_mvr(
  data,
  last_factor = NULL,
  summary_by = NULL,
  groups = NULL,
  variables = NULL
)
```

## Arguments

- data:

  Field book data.

- last_factor:

  The last factor in your fieldbook `[string: NULL]`.

- summary_by:

  Variables for group the analysis.

- groups:

  Groups for color in PCA.

- variables:

  Variables to be use in the analysis `[string: NULL]`.

## Value

result and plots

## Details

Compute and plot information for multivariate analysis (PCA, HCPC and
correlation).

## Examples

``` r
if (FALSE) { # \dontrun{

library(inti)

fb <- inti::potato 

mv <- yupana_mvr(data = fb
                 , last_factor = "geno"
                 , summary_by = c("geno", "treat")
                 , groups = "treat"
                 , variables = c("all")
                 #, variables = c("wue", "twue")
                 )
                 
mv$plot[1] 

mv$data


} # }
```
