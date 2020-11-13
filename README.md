
<!-- README.md is generated from README.Rmd. Please edit that file -->

# inti <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/Flavjack/inti.svg?branch=master)](https://travis-ci.org/Flavjack/inti)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/Flavjack/inti?branch=master&svg=true)](https://ci.appveyor.com/project/Flavjack/inti)
[![CRAN
status](https://www.r-pkg.org/badges/version/inti)](https://CRAN.R-project.org/package=inti)
<!-- badges: end -->

The ‘inti’ package is part of the ‘inkaverse’ project for developing
different procedures and tools used in plant science and experimental
designs. The mean aim of the package is to support researchers during
the planning of experiments and data collection ‘tarpuy()’, data
analysis and graphics ‘yupana()’, and technical writing ‘rtciles()’.
Learn more about the ‘inkaverse’ project at <https://inkaverse.com/>.

## Installation

To install the stable version from [CRAN](https://CRAN.R-project.org),
run the following from an R console:

``` r
install.packages("inti")
```

To install the latest development version directly from
[GitHub](https://github.com/Flavjack/inti), run the following from an R
console:

``` r
if (!require("remotes"))
  install.packages("remotes")
remotes::install_github("flavjack/inti")
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

### Rticles

``` r
inti::rticles()
```
