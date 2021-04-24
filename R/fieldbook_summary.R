#' Fieldbook summary
#'
#' Function to deploy a summary of the variables
#'
#' @param data Field book data.
#' @param last_factor The last factor in your fieldbook.
#' @param model_facts Model used for the experimental design.
#' @param comp_facts Factors to compare. See details.
#' @param test_comp Comparison test (default = "SNK"). Others: "TUKEY",
#'   "DUNCAN".
#' @param sig_level Significance level for the analysis (default = 0.05).
#'
#' @details
#'
#' For compare the factors you should use "*". For example, to compare
#' treatment1 and treatment2: \code{treatment1*treatment2}.
#'
#' @return data frame
#'
#' @import dplyr
#' @import tidyr
#'
#' @export
#'

fieldbook_summary <- function(data
                             , last_factor
                             , model_facts
                             , comp_facts
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
    mutate(comp_facts = {{comp_facts}}) %>%
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
    summarise(levels = n_distinct(values, na.rm = TRUE)) %>%
    mutate(type = "factor", .after = "variables")

# combine tables ----------------------------------------------------------
# -------------------------------------------------------------------------

  fb_smr <- bind_rows(fact_table, var_table) %>% 
    mutate(plot_label = .data$variables)
  

# result ------------------------------------------------------------------
# -------------------------------------------------------------------------

  return(fb_smr)

}
