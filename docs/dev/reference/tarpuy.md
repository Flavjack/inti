# Interactive fieldbook designs

Invoke RStudio addin to create fieldbook designs

## Usage

``` r
tarpuy(dependencies = FALSE)
```

## Arguments

- dependencies:

  Logical. If `TRUE`, install any missing TARPUY runtime dependencies
  using the local `inst/tarpuy/setup.R` script included with the
  installed `inti` package.

## Value

A Shiny application launched with
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Details

TARPUY allows users to create experimental designs through an
interactive Shiny application.

## Examples

``` r

if (interactive()) {
  inti::tarpuy()
}
```
