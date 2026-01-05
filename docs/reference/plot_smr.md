# Plot summary data

Graph summary data into bar o line plot

## Usage

``` r
plot_smr(
  data,
  type = NULL,
  x = NULL,
  y = NULL,
  group = NULL,
  xlab = NULL,
  ylab = NULL,
  glab = NULL,
  ylimits = NULL,
  xrotation = c(0, 0.5, 0.5),
  xtext = NULL,
  gtext = NULL,
  legend = "top",
  sig = NULL,
  sigsize = 3,
  error = NULL,
  color = TRUE,
  opt = NULL
)
```

## Arguments

- data:

  Output from summary data

- type:

  Type of graphic. "bar" or "line"

- x:

  Axis x variable

- y:

  Axis y variable

- group:

  Group variable

- xlab:

  Title for the axis x

- ylab:

  Title for the axis y

- glab:

  Title for the legend

- ylimits:

  limits of the y axis c(initial, end, brakes)

- xrotation:

  Rotation in x axis c(angle, h, v)

- xtext:

  Text labels in x axis using a vector

- gtext:

  Text labels in group using a vector

- legend:

  the position of legends ("none", "left", "right", "bottom", "top", or
  two-element numeric vector)

- sig:

  Column with the significance

- sigsize:

  Font size in significance letters

- error:

  Show the error bar ("ste" or "std")

- color:

  colored figure (TRUE), black & white (FALSE) or color vector

- opt:

  Add news layer to the plot

## Value

plot

## Details

If the table is a out put of `mean_comparison(graph_opts = TRUE)`
function. Its contain all the parameter for the plot.

You could add additional layer to the plot using "+" with ggplot2
options

## Examples

``` r
if (FALSE) { # \dontrun{

library(inti)

fb <- potato

yrs <- yupana_analysis(data = fb
                       , response = "hi"
                       , model_factors = "geno*treat"
                       , comparison = c("geno", "treat")
                       )

yrs$meancomp %>% 
  plot_smr(type = "bar"
           , x = "geno"
           , y = "hi"
           , xlab = ""
           , group = "treat"
           , glab = "Tratamientos"
           , error = "ste"
           , sig = "sig"
           #, ylimits = c(0, 1, 0.2)
           , color = T #c("red", "black")
           , gtext = c("Irrigado", "Sequia")
           )
           
yrs$meancomp %>% 
  plot_smr(type = "bar"
           , x = "treat"
           , y = "hi"
           , group = "geno"
           , glab = "Genotipo"
           , error = "ste"
           , sig = "sig"
           )
           
} # }
```
