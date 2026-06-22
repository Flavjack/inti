# inti

[![CRAN](https://img.shields.io/cran/v/inti.png)](https://cran.r-project.org/package=inti)
[![DOI](https://zenodo.org/badge/82401374.svg)](https://zenodo.org/badge/latestdoi/82401374)
[![R-CMD-check](https://github.com/Flavjack/inti/workflows/R-CMD-check/badge.svg)](https://github.com/Flavjack/inti/actions)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/inti?color=brightgreen.png)](https://r-pkg.org/pkg/inti)

The **inti** package is part of the **inkaverse** project, which
develops tools and workflows for plant science, experimental design,
data analysis, and scientific writing. The package supports researchers
during experiment planning and field data collection through **Tarpuy**,
statistical analysis and visualization through **Yupana**, and the
preparation of scientific outputs.

More information about the project is available at
<https://inkaverse.com/>.

## Installation

The stable version of **inti** can be installed from CRAN:

``` r

install.packages("inti")
```

To install the latest development version directly from GitHub, it is
recommended to use **pak**:

``` r

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pak::pkg_install("flavjack/inti")
```

To install a specific version from CRAN (e.g., version 0.4.4):

``` r

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pak::pkg_install("inti@0.4.4")
```

After installation, load the package:

``` r

library(inti)
```

## Shiny Applications

The **inti** package includes two Shiny applications that provide
graphical interfaces for experimental design, data management,
statistical analysis, and result visualization.

### Installing Application Dependencies

The first time you run any of the applications, install the required
dependencies:

``` r

inti::yupana(dependencies = TRUE)
```

This step only needs to be performed once.

### Launching the Applications

The applications can be accessed directly from the **RStudio Addins**
menu or launched from the R console.

**Yupana** is an interactive environment for statistical analysis and
visualization of experimental data.

``` r

inti::yupana()
```

**Tarpuy** is an interactive platform for experimental planning,
field-book generation, treatment organization, and data management.

``` r

inti::tarpuy()
```
