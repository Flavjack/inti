# Remove outliers using mixed models

Use the method M4 in Bernal Vasquez (2016). Bonferroni Holm test to
judge residuals standardized by the re scaled MAD (BH MADR).

## Usage

``` r
remove_outliers(
  data,
  formula,
  drop_na = FALSE,
  plot_diag = FALSE,
  title = "Diagnostic Plot"
)
```

## Arguments

- data:

  Experimental design data frame with the factors and traits.

- formula:

  mixed model formula.

- drop_na:

  drop NA values from the data.frame

- plot_diag:

  Diagnostic plot based in the raw and clean data

- title:

  Plot title

## Value

list. 1. Table with date without outliers. 2. The outliers in the
dataset.

## Details

Function to remove outliers in MET experiments

## References

Bernal Vasquez, Angela Maria, et al. “Outlier Detection Methods for
Generalized Lattices: A Case Study on the Transition from ANOVA to
REML.” Theoretical and Applied Genetics, vol. 129, no. 4, Apr. 2016.

## Examples

``` r

library(inti)

rmout <- potato %>%
  remove_outliers(data = .
  , formula = stemdw ~ 0 + (1|bloque) + treat*geno
  , plot_diag = TRUE
  , drop_na = FALSE
  , title = "Plot Diagnostic"
  )
#> Error in remove_outliers(data = ., formula = stemdw ~ 0 + (1 | bloque) +     treat * geno, plot_diag = TRUE, drop_na = FALSE, title = "Plot Diagnostic"): unused argument (title = "Plot Diagnostic")

rmout
#> Error: object 'rmout' not found
  
```
