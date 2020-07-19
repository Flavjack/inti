#' Field book variable list
#'
#' Function to include the variables to evaluate in the fieldbook design.
#'
#' @param varlits Data frame with the variables information. See examples.
#' @param fblength Number of row in the fieldbook  (default = NULL). See details.
#' @param abbreviation Column contain the variables abbreviations (default = "abbreviation"). See details.
#' @param evaluation  Column contain when the variables will be made (default = "evaluation").
#' @param sampling  Column contain the number of sampling in each experimental unit (default = "sampling"). See details.
#'
#' @details The function allows to include the arguments in the sheet that have the information of the variables.
#' You should include 3 columns in the sheet: "\code{{abbreviation}}", "\code{{evaluation}}" and "\code{{sampling}}". See examples.
#' The information will be extracted automatically and deploy the list of variable for the fieldbook design.
#' If you will be evaluate the experimental unit one time you should set 1.
#'
#' @return data frame
#'
#' @author
#'
#' Flavio Lozano-Isla
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#'
#' @examples
#'
#' \dontrun{
#'
#' library(googlesheets4)
#' library(tidyverse)
#' library(inti)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "1ilw0NHT7mihaM-3U48KzkuMt927xe8ukX6rNuIw2fT0/edit#gid=4349630")
#'
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (varlits <- gs %>%
#'     range_read("variables"))
#'
#' fb <- varlits %>% fieldbook_varlist(fblength = 30)
#'
#' }
#'
#' @export

fieldbook_varlist <- function(varlits
                              , fblength
                              , abbreviation = "abbreviation"
                              , evaluation = "evaluation"
                              , sampling = "sampling"
                              ) {

  blank <- NULL

  abrv_match <- match.arg(abbreviation, c("abbreviation", "siglas"))
  eval_match <- match.arg(evaluation, c("evaluation", "eval", "dap", "dat"))
  smp_match <- match.arg(sampling, c("sampling", "sample", "subplot", "muestra"))

  data <- varlits %>%
    select( starts_with("{") |  ends_with("}") ) %>%
    rename_with(~ gsub("\\{|\\}", "", .)) %>%
    drop_na()

  smp_n <- data[[smp_match]]

  var_list <- data %>%
    tidyr::uncount(smp_n, .id = "sample") %>%
    select(-sampling) %>%
    unite("var_list", {{abrv_match}}, {{eval_match}}, sample , sep = "_") %>%
    mutate(blank = NA) %>%
    pivot_wider(names_from = var_list, values_from = blank) %>%
    tibble(.rows = fblength)

  var_list

}
