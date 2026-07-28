# Transform fieldbooks based in a dictionary

Transform entire fieldbook according to data a dictionary

## Usage

``` r
metamorphosis(fieldbook, dictionary, from, to, index, colnames)
```

## Arguments

- fieldbook:

  Data frame with the original information.

- dictionary:

  Data frame with new names and categories. See details.

- from:

  Column of the dictionary with the original names.

- to:

  Column of the dictionary with the new names.

- index:

  Column of the dictionary with the type and level of the variables.

- colnames:

  Character vector with the name of the columns.

## Value

List with two objects. 1. New data frame. 2. Dictionary.

## Details

The function require at least three columns.

1.  Original names (`from`).

2.  New names (`to`).

3.  Variable type (`index`).
