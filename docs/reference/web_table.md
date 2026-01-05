# HTML tables for markdown documents

Export tables with download, pasta and copy buttons

## Usage

``` r
web_table(
  data,
  caption = NULL,
  digits = 2,
  rnames = FALSE,
  buttons = NULL,
  file_name = "file",
  scrolly = NULL,
  columnwidth = "200px",
  width = "100%"
)
```

## Arguments

- data:

  Dataset.

- caption:

  Title for the table.

- digits:

  Digits number in the table exported.

- rnames:

  Row names.

- buttons:

  Buttons: "excel", "copy" or "none". Default c("excel", "copy")

- file_name:

  Excel file name

- scrolly:

  Windows height to show the table. Default "45vh"

- columnwidth:

  Column width. Default '200px'

- width:

  Width in pixels or percentage (Defaults to automatic sizing)

## Value

table in markdown format for html documents

## Examples

``` r
if (FALSE) { # \dontrun{

library(inti)

met %>%
  web_table(caption = "Web table")

} # }
```
