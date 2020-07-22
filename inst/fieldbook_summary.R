
library(googlesheets4)
library(tidyverse)

url <- paste0("https://docs.google.com/spreadsheets/d/"
, "15uwCgQRtR01B3FJaZBE8t_0bOC_8Bbey9ccwVlZH0jg/edit#gid=56711214")
# browseURL(url)
gs <- as_sheets_id(url)

(data <- gs %>%
    range_read("fb"))

data %>%


fielbook_summary <- function(data
                             , factors
                             , model
                             , comparison
                             , test.comp = "SNK"
                             , sig.level = 0.05
                             ) {

  factors <- 6
  model <- "bloque + cultivar*fuenteN*dosis"
  comparison <- "cultivar:fuenteN:dosis"

# var list ----------------------------------------------------------------
# -------------------------------------------------------------------------

  fact_names <- data %>%
    dplyr::select(1:{{factors}}) %>%
    names()

  var_names <- data %>%
    dplyr::select(!{{fact_names}}) %>%
    names()

# variables table ---------------------------------------------------------
# -------------------------------------------------------------------------

  var_table <- data %>%
    select({{var_names}}) %>%
    pivot_longer(
      cols = {{var_names}}
      , names_to = "variables"
      ) %>%
    group_by(variables) %>%
    summarise(
      values = sum(!is.na(value)),
      NAs = sum(is.na(value)),
      ) %>%
    mutate(model = {{model}} ) %>%
    mutate(comparison = {{comparison}}) %>%
    mutate(test.comp =  {{test.comp}} ) %>%
    mutate(sig.level = {{sig.level}} ) %>%
    mutate(export = "yes")

# table factors -----------------------------------------------------------
# -------------------------------------------------------------------------

  fact_table <- data %>%
    select({{fact_names}}) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), as.factor)) %>%
    pivot_longer(
      cols = {{fact_names}}
      , names_to = "variables"
    ) %>%
    group_by(variables) %>%
    summarise(values = n_distinct(value))

# combine tables ----------------------------------------------------------
# -------------------------------------------------------------------------

  fb_smr <- bind_rows(fact_table, var_table)

# result ------------------------------------------------------------------
# -------------------------------------------------------------------------

  return(fb_smr)

}
