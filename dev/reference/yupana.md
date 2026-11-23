# Interactive data analysis

Invoke RStudio addin to analyze and graph experimental design data

## Usage

``` r
yupana(dependencies = FALSE, port = 1221)
```

## Arguments

- dependencies:

  Install package dependencies for run the app

- port:

  Port on which the app runs `[numeric: 1221]`. The default matches the
  port registered for Google sign-in. Use another value (or `NULL` for a
  random free port) if 1221 is busy, e.g. when TARPUY is already
  running; Google sign-in may not work on other ports.

## Value

Shiny app

## Details

Yupana: data analysis and graphics for experimental designs.

## Examples

``` r

if(interactive()){

 inti::yupana()

}
```
