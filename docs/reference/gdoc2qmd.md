# Google docs to Rmarkdown

Use Articul8 Add-ons from Google docs to build Rticles

## Usage

``` r
gdoc2qmd(file, export = NA, format = "qmd", type = "asis", fill_table = "down")
```

## Arguments

- file:

  Zip file path from Articul8 exported in md format `[path]`

- export:

  Path to export the files `[path: NA (file directory)]`

- format:

  Output format `[character: "qmd", "rmd"]`

- type:

  output file type `[character: "asis" "list", "listfull", "full"]`

- fill_table:

  extract table in listfull `[character: "up", "down"]`

## Value

path

## Details

Document rendering until certain point: "#\| end" Include for next page:
"#\| newpage" You can include the cover page params using "#\|" in a
Google docs table
