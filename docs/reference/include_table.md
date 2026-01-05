# Table with footnotes

Include tables with title and footnotes for word and html documents

## Usage

``` r
include_table(table, caption = NA, notes = NA, label = NA, notation = "none")
```

## Arguments

- table:

  Data frame.

- caption:

  Table caption (default = NULL). See details.

- notes:

  Footnotes for the table (default = NA). See details.

- label:

  Label for start the footnote (default = NA).

- notation:

  Notation for the symbols and footnotes (default = "none") Others:
  "alphabet", "number", "symbol".

## Value

Table with caption and footnotes

## Examples

``` r
library(inti)

table <- data.frame(
x = rep_len(1, 5)
, y = rep_len(3, 5)
, z = rep_len("c", 5)
)

table %>% inti::include_table(
  caption = "Title caption b) line 0
  a) line 1
  b) line 2"
  , notes = "Footnote"
  , label = "Where:"
  )
#> 
#> 
#> Table: Title caption b) line 0 a) line 1 b) line 2
#> 
#> |  x|  y|z  |
#> |--:|--:|:--|
#> |  1|  3|c  |
#> |  1|  3|c  |
#> |  1|  3|c  |
#> |  1|  3|c  |
#> |  1|  3|c  |
#> 
#> <small>Where:</small>
#> <small>Footnote</small>
  
```
