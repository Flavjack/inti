# Diagnostic plots

Function to plot the diagnostic of models

## Usage

``` r
plot_diag(model, title = NA)
```

## Arguments

- model:

  Statistical model

- title:

  Plot title

## Value

plots

## Examples

``` r
if (FALSE) { # \dontrun{

library(inti)

lm <- aov(stemdw ~ bloque + geno*treat, data = potato)

# lm <- potato %>% lme4::lmer(stemdw ~ (1|bloque) + geno*treat, data = .)
 
plot(lm, which = 1)
plot_diag(lm)[3]

plot(lm, which = 2)
plot_diag(lm)[2]

plot(lm, which = 3)
plot_diag(lm)[4]

plot(lm, which = 4)
plot_diag(lm)[1]

} # }
```
