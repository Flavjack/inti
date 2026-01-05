# Diagnostic plots

Function to plot the diagnostic of models

## Usage

``` r
plot_diagnostic(data, formula, title = NA)
```

## Arguments

- data:

  Experimental design data frame with the factors and traits.

- formula:

  Mixed model formula

- title:

  Plot title

## Value

plots

## Examples

``` r
if (FALSE) { # \dontrun{

library(inti)

plot_diagnostic(data = potato
                , formula = stemdw ~ (1|bloque) + geno*treat)

} # }
```
