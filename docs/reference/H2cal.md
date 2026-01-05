# Broad-sense heritability in plant breeding

Heritability in plant breeding on a genotype difference basis

## Usage

``` r
H2cal(
  data,
  trait,
  gen.name,
  rep.n,
  env.n = 1,
  year.n = 1,
  env.name = NULL,
  year.name = NULL,
  fixed.model,
  random.model,
  summary = FALSE,
  emmeans = FALSE,
  weights = NULL,
  plot_diag = FALSE,
  outliers.rm = FALSE,
  trial = NULL
)
```

## Arguments

- data:

  Experimental design data frame with the factors and traits.

- trait:

  Name of the trait.

- gen.name:

  Name of the genotypes.

- rep.n:

  Number of replications in the experiment.

- env.n:

  Number of environments (default = 1). See details.

- year.n:

  Number of years (default = 1). See details.

- env.name:

  Name of the environments (default = NULL). See details.

- year.name:

  Name of the years (default = NULL). See details.

- fixed.model:

  The fixed effects in the model (BLUEs). See examples.

- random.model:

  The random effects in the model (BLUPs). See examples.

- summary:

  Print summary from random model (default = FALSE).

- emmeans:

  Use emmeans for calculate the BLUEs (default = FALSE).

- weights:

  an optional vector of ‘prior weights’ to be used in the fitting
  process (default = NULL).

- plot_diag:

  Show diagnostic plots for fixed and random effects (default = FALSE).
  Options: "base", "ggplot". .

- outliers.rm:

  Remove outliers (default = FALSE). See references.

- trial:

  Column with the name of the trial in the results (default = NULL).

## Value

list

## Details

The function allows to made the calculation for individual or
multi-environmental trials (MET) using fixed and random model.

1.  The variance components based in the random model and the population
    summary information based in the fixed model (BLUEs).

2.  Heritability under three approaches: Standard (ANOVA), Cullis
    (BLUPs) and Piepho (BLUEs).

3.  Best Linear Unbiased Estimators (BLUEs), fixed effect.

4.  Best Linear Unbiased Predictors (BLUPs), random effect.

5.  Table with the outliers removed for each model.

For individual experiments is necessary provide the `{trait}`,
`{gen.name}`, `{rep.n}`.

For MET experiments you should `{env.n}` and `{env.name}` and/or
`{year.n}` and `{year.name}` according your experiment.

The BLUEs calculation based in the pairwise comparison could be time
consuming with the increase of the number of the genotypes. You can
specify `{emmeans = FALSE}` and the calculate of the BLUEs will be
faster.

If `{emmeans = FALSE}` you should change 1 by 0 in the fixed model for
exclude the intersect in the analysis and get all the genotypes BLUEs.

For more information review the references.

## References

Bernal Vasquez, Angela Maria, et al. “Outlier Detection Methods for
Generalized Lattices: A Case Study on the Transition from ANOVA to
REML.” Theoretical and Applied Genetics, vol. 129, no. 4, Apr. 2016.

Buntaran, H., Piepho, H., Schmidt, P., Ryden, J., Halling, M., and
Forkman, J. (2020). Cross validation of stagewise mixed model analysis
of Swedish variety trials with winter wheat and spring barley. Crop
Science, 60(5).

Schmidt, P., J. Hartung, J. Bennewitz, and H.P. Piepho. 2019.
Heritability in Plant Breeding on a Genotype Difference Basis. Genetics
212(4).

Schmidt, P., J. Hartung, J. Rath, and H.P. Piepho. 2019. Estimating
Broad Sense Heritability with Unbalanced Data from Agricultural Cultivar
Trials. Crop Science 59(2).

Tanaka, E., and Hui, F. K. C. (2019). Symbolic Formulae for Linear Mixed
Models. In H. Nguyen (Ed.), Statistics and Data Science. Springer.

Zystro, J., Colley, M., and Dawson, J. (2018). Alternative Experimental
Designs for Plant Breeding. In Plant Breeding Reviews. John Wiley and
Sons, Ltd.

## Author

Maria Belen Kistner

Flavio Lozano Isla

## Examples

``` r
library(inti)

dt <- potato

hr <- H2cal(data = dt
            , trait = "stemdw"
            , gen.name = "geno"
            , rep.n = 5
            , fixed.model = ~ 0 + (1|bloque) + geno
            , random.model = ~ 1 + (1|bloque) + (1|geno)
            , emmeans = TRUE
            , plot_diag = FALSE
            , outliers.rm = TRUE
            )

 hr$tabsmr
#>    trait rep geno env year     mean      std   min    max      V.g      V.e
#> 1 stemdw   5   15   1    1 12.59867 4.749994 2.818 22.302 19.96002 9.410932
#>        V.p repeatability     H2.s      H2.p      H2.c
#> 1 21.84221      0.913828 0.913828 0.9502395 0.9533473
 hr$blues
#> # A tibble: 15 × 6
#>    geno  stemdw    SE    df lower.CL upper.CL
#>    <fct>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1 G01    15.7   1.03  120.   13.7      17.8 
#>  2 G02    10.1   1.03  120.    8.08     12.2 
#>  3 G03     9.70  1.03  120.    7.65     11.7 
#>  4 G04    15.2   1.03  120.   13.1      17.2 
#>  5 G05    12.9   1.09  123.   10.7      15.0 
#>  6 G06    22.3   1.03  120.   20.3      24.3 
#>  7 G07     2.82  1.03  120.    0.778     4.86
#>  8 G08    10.4   1.03  120.    8.38     12.5 
#>  9 G09    15.7   1.03  120.   13.6      17.7 
#> 10 G10     9.24  1.03  120.    7.20     11.3 
#> 11 G11     6.43  1.03  120.    4.38      8.47
#> 12 G12    16.1   1.03  120.   14.1      18.2 
#> 13 G13    14.6   1.03  120.   12.6      16.7 
#> 14 G14    16.3   1.03  120.   14.3      18.3 
#> 15 G15    11.5   1.03  120.    9.43     13.5 
 hr$blups
#> # A tibble: 15 × 2
#>    geno  stemdw
#>    <chr>  <dbl>
#>  1 G01    15.6 
#>  2 G02    10.2 
#>  3 G03     9.82
#>  4 G04    15.1 
#>  5 G05    12.8 
#>  6 G06    20.6 
#>  7 G07     3.25
#>  8 G08    10.5 
#>  9 G09    15.5 
#> 10 G10     9.39
#> 11 G11     6.70
#> 12 G12    15.9 
#> 13 G13    14.5 
#> 14 G14    16.1 
#> 15 G15    11.5 
 hr$outliers
#> $fixed
#>    index bloque geno stemdw     resi  res_MAD rawp.BHStud adjp bholm out_flag
#> 68    68     IV  G05  80.65 60.36709 18.84505           0    0     0  OUTLIER
#> 
#> $random
#>     index bloque geno stemdw     resi   res_MAD  rawp.BHStud         adjp
#> 68     68     IV  G05  80.65 61.39925 18.886676 0.0000000000 0.0000000000
#> 100   100     IV  G06  33.52 12.02340  3.698449 0.0002169207 0.0002169207
#>          bholm out_flag
#> 68  0.00000000  OUTLIER
#> 100 0.03232119  OUTLIER
#> 
 
```
