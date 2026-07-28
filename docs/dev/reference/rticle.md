# Convert a Markdown manuscript into a Quarto document

Reads a Markdown file exported from Google Docs and converts it into a
Quarto-compatible manuscript. The function detects and separates text,
figures, and tables, removes formatting artifacts, restores equations
embedded as image placeholders, and inserts format-specific section
breaks. The resulting document can be rendered directly using Quarto for
HTML, Word, or PDF outputs.

## Usage

``` r
rticle(file = "draft.md", export = "files", type = c("asis", "list"))
```

## Arguments

- file:

  Character string indicating the path to the Markdown file. Default is
  "draft.md".

- export:

  Character string specifying the output directory where the generated
  .qmd file will be saved. If NULL, the directory name is derived from
  the input file name.

- type:

  Character string indicating how the manuscript should be organized.
  "asis" preserves the original structure, whereas "list" rearranges the
  content into text, figures, and tables.

## Value

A character vector containing the full path of the generated Quarto
(.qmd) file.
