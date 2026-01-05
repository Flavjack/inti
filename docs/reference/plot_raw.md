# Plot raw data

Function use the raw data for made a boxplot graphic

## Usage

``` r
plot_raw(
  data,
  type = "boxplot",
  x,
  y,
  group = NULL,
  xlab = NULL,
  ylab = NULL,
  glab = NULL,
  ylimits = NULL,
  xlimits = NULL,
  xrotation = NULL,
  legend = "top",
  xtext = NULL,
  gtext = NULL,
  color = TRUE,
  linetype = 1,
  opt = NULL
)
```

## Arguments

- data:

  raw data

- type:

  Type of graphic. "boxplot" or "scatterplot"

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

  Limits and break of the y axis c(initial, end, brakes)

- xlimits:

  For scatter plot. Limits and break of the x axis c(initial, end,
  brakes)

- xrotation:

  Rotation in x axis c(angle, h, v)

- legend:

  the position of legends ("none", "left", "right", "bottom", "top", or
  two-element numeric vector)

- xtext:

  Text labels in x axis using a vector

- gtext:

  Text labels in groups using a vector

- color:

  Colored figure (TRUE), black & white (FALSE) or color vector

- linetype:

  Line type for regression. Default = 0

- opt:

  Add new layers to the plot

## Value

plot

## Details

You could add additional layer to the plot using "+" with ggplot2
options

## Examples

``` r
if (FALSE) { # \dontrun{

library(inti)

fb <- potato

fb %>%
  plot_raw(type = "box"
           , x = "geno"
           , y = "twue"
           #, group = "treat"
           , ylab = NULL
           , xlab = NULL
           , glab = ""
           ) 
           
fb %>%
  plot_raw(type = "sca"
           , x = "hi"
           , y = "twue"
           , group = "geno"
           ) 
           
} # }
```
