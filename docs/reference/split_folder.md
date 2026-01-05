# Split folder

Function to split folder by size or number of elements

## Usage

``` r
split_folder(
  folder,
  export,
  units = "megas",
  size = 500,
  zip = TRUE,
  remove = FALSE
)
```

## Arguments

- folder:

  Path of folder to split (path).

- export:

  Path to export the split folders (path).

- units:

  Units to split folder (string: "megas", "number").

- size:

  Folder size by the units selected (numeric).

- zip:

  Zip split folders (logical).

- remove:

  Remove the split folder after zip (logical).

## Value

zip files

## Examples

``` r
if (FALSE) { # \dontrun{

split_folder("pictures/QUINOA 2018-2019 SC SEEDS EDWIN - CAMACANI/"
   , "pictures/split_num", remove = T, size = 400, units = "number")

} # }
```
