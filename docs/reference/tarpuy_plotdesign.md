# Fieldbook plot experimental designs

Plot fieldbook sketch designs based in experimental design

## Usage

``` r
tarpuy_plotdesign(
  data,
  factor = NA,
  fill = "plots",
  xlab = NULL,
  ylab = NULL,
  glab = NULL
)
```

## Arguments

- data:

  Experimental design data frame with the factors and level. See
  examples.

- factor:

  Vector with the name of the columns with the factors.

- fill:

  Value for fill the experimental units (default = "plots").

- xlab:

  Title for x axis.

- ylab:

  Title for y axis.

- glab:

  Title for group axis.

## Value

plot

## Details

The function allows to plot the experimental design according the field
experiment design.

## Examples

``` r
if (FALSE) { # \dontrun{

library(inti)
library(gsheet)

url <- paste0("https://docs.google.com/spreadsheets/d/"
              , "1_BVzChX_-lzXhB7HAm6FeSrwq9iKfZ39_Sl8NFC6k7U/edit#gid=1834109539")
# browseURL(url)

fb <- gsheet2tbl(url) 

dsg <- fb %>% tarpuy_design() 

dsg

dsg %>% str()

dsg %>% 
  tarpuy_plotdesign()

} # }
```
