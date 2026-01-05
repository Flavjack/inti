# Footnotes in tables

Include tables footnotes and symbols for kables in pandoc format

## Usage

``` r
footnotes(table, notes = NULL, label = "Note:", notation = "alphabet")
```

## Arguments

- table:

  Kable output in pandoc format.

- notes:

  Footnotes for the table.

- label:

  Label for start the footnote.

- notation:

  Notation for the footnotes (default = "alphabet"). See details.

## Value

Table with footnotes for word and html documents

## Details

You should use the pandoc format `kable(format = "pipe")`. You can add
the footnote symbol using `{hypen}` in your table. `notation` could be
use: "alphabet", "number", "symbol", "none".
