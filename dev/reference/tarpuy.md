# Interactive fieldbook designs

Invoke RStudio addin to create fieldbook designs

## Usage

``` r
tarpuy(dependencies = FALSE, port = 1221)
```

## Arguments

- dependencies:

  Logical. If `TRUE`, install any missing TARPUY runtime dependencies
  using the local `inst/tarpuy/setup.R` script included with the
  installed `inti` package.

- port:

  Port on which the app runs `[numeric: 1221]`. The default matches the
  port registered for Google sign-in. Use another value (or `NULL` for a
  random free port) if 1221 is busy, e.g. when YUPANA is already
  running; Google sign-in may not work on other ports.

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
