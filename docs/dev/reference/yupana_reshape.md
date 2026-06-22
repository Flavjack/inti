# Fieldbook reshape

Function to reshape fieldbook according a separation character

## Usage

``` r
yupana_reshape(
  data,
  last_factor,
  sep,
  new_colname,
  from_var = NULL,
  to_var = NULL,
  exc_factors = NULL
)
```

## Arguments

- data:

  Field book raw data.

- last_factor:

  The last factor in your field book.

- sep:

  Character that separates the last value.

- new_colname:

  The new name for the column created.

- from_var:

  The first variable in case you want to exclude several. variables.

- to_var:

  The last variable in case you want to exclude several variables.

- exc_factors:

  Factor to exclude during the reshape.

## Value

data frame

## Details

If you variable name is `variable_evaluation_rep`. The reshape function
will help to create the column `rep` and the new variable name will be
`variable_evaluation`.
