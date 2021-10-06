
<!-- README.md is generated from README.Rmd. Please edit that file -->

# inti <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/inti)](https://CRAN.R-project.org/package=inti)
[![DOI](https://zenodo.org/badge/82401374.svg)](https://zenodo.org/badge/latestdoi/82401374)
[![R-CMD-check](https://github.com/Flavjack/inti/workflows/R-CMD-check/badge.svg)](https://github.com/Flavjack/inti/actions)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/inti?color=brightgreen)](https://r-pkg.org/pkg/inti)
<!-- badges: end -->

The ‘inti’ package is part of the ‘inkaverse’ project for developing
different procedures and tools used in plant science and experimental
designs. The mean aim of the package is to support researchers during
the planning of experiments and data collection ‘tarpuy()’, data
analysis and graphics ‘yupana()’, and technical writing. Learn more
about the ‘inkaverse’ project at <https://inkaverse.com/>.

## Installation

To install the stable version from
[CRAN](https://cran.r-project.org/package=inti):

``` r
install.packages("inti")
```

To install the latest development version directly from
[GitHub](https://github.com/flavjack/inti):

``` r
if (!require("remotes"))
  install.packages("remotes")
remotes::install_github("flavjack/inti")
```

If you need install an specific version of a package:

``` r
if (!require("remotes"))
  install.packages("remotes")
remotes::install_version("inti", version = "0.4.4")
```

## Shiny apps

If is the first time running any of the apps consider install the app
dependencies:

``` r
inti::yupana(dependencies = TRUE)
```

After install the package and the app dependencies also you can access
to the apps through the Addins list in Rstudio or running the following
code:

### Yupana

``` r
inti::yupana()
```

### Tarpuy

``` r
inti::tarpuy()
```
