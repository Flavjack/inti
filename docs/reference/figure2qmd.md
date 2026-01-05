# Figure to Quarto format

Use Articul8 Add-ons from Google docs to build Rticles

## Usage

``` r
figure2qmd(text, path = ".", opts = NA)
```

## Arguments

- text:

  Markdown text with figure information `[string]`

- path:

  Image path for figures `[path: "." (base directory)]`

- opts:

  chunk options in brackets `[string: NA]`

## Value

string mutated

## Details

Quarto option can be included in the title using `{{}}` separated by
commas
