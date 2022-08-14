source("https://inkaverse.com/setup.r")

url <- "https://docs.google.com/spreadsheets/d/15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/edit#gid=1578817386"

gs <- url %>% 
  as_sheets_id()

data <- gs %>% 
  range_read("test")

last_factor <- "bloque"
sep <- "."
new_colname <- "sample"
from_var = NULL
to_var = NULL
exc_factors = NULL

