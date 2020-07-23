#' Field book summary
#'
#' Function to deploy a summary of the variables
#'
#' @param data Field book data.
#' @param last_factor The last factor in your fieldbook.
#' @param model_facts Model used for the experimental design.
#' @param treat_comp Factors to compare. See details.
#' @param test_comp Comparison test (default = "SNK"). Others: "TUKEY" & "DUNCAN".
#' @param sig_level Significance level for the analysis (default = 0.05).
#'
#' @details
#'
#' For compare the factors you should use ":". For example, to compare treatment1
#' and treatment2 => "treatment1:treatment2".
#'
#' @return data frame
#'
#' @author
#'
#' Flavio Lozano-Isla
#'
#' @import dplyr
#' @import tidyr
#'
#' @examples
#'
#' \dontrun{
#'
#' library(inti)
#' library(googlesheets4)
#' library(tidyverse)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "15uwCgQRtR01B3FJaZBE8t_0bOC_8Bbey9ccwVlZH0jg/edit#gid=56711214")
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (data <- gs %>%
#'     range_read("fb"))
#'
#' smrfb <- fielbook_summary(data
#'                  , last_factor = "dosis"
#'                  , model_facts = "bloque + cultivar*fuenteN*dosis"
#'                  , treat_comp = "cultivar:fuenteN:dosis"
#'                  )
#' smrfb
#'
#' smrfb %>% write_sheet(ss = gs, sheet = "fbsmr")
#'
#' }
#'
#' @export

fielbook_summary <- function(data
                             , last_factor
                             , model_facts
                             , treat_comp
                             , test_comp = "SNK"
                             , sig_level = 0.05
                             ) {

  variables <- values <- NULL

# var list ----------------------------------------------------------------
# -------------------------------------------------------------------------

  fact_names <- data %>%
    dplyr::select(1:{{last_factor}}) %>%
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
      , values_to = "values"
      ) %>%
    group_by(variables) %>%
    summarise(
      levels = sum(!is.na(values)),
      NAs = sum(is.na(values)),
      ) %>%
    mutate(model_facts = {{model_facts}} ) %>%
    mutate(treat_comp = {{treat_comp}}) %>%
    mutate(test_comp =  {{test_comp}} ) %>%
    mutate(sig_level = {{sig_level}} ) %>%
    mutate(type = "numeric", .after = "variables")

# table factors -----------------------------------------------------------
# -------------------------------------------------------------------------

  fact_table <- data %>%
    select({{fact_names}}) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), as.factor)) %>%
    pivot_longer(
      cols = {{fact_names}}
      , names_to = "variables"
      , values_to = "values"
    ) %>%
    group_by(variables) %>%
    summarise(levels = n_distinct(values)) %>%
    mutate(type = "factor", .after = "variables")

# combine tables ----------------------------------------------------------
# -------------------------------------------------------------------------

  fb_smr <- bind_rows(fact_table, var_table)

# result ------------------------------------------------------------------
# -------------------------------------------------------------------------

  return(fb_smr)

}
