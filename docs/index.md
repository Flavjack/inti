# inti

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

If you need install an specific version:

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
