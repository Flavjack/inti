#' Field book variable list
#'
#' Function to include the variables to evaluate in the fieldbook design.
#'
#' @param fieldbook Data frame with the fieldbook.
#' @param varlist Data frame with the variables information. See examples.
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
#' library(inti)
#' library(googlesheets4)
#' library(tidyverse)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "1ilw0NHT7mihaM-3U48KzkuMt927xe8ukX6rNuIw2fT0/edit#gid=0")
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (data <- gs %>%
#'     range_read("tarpuyr"))
#'
#' (varlits <- gs %>%
#'     range_read("variables"))
#'
#' fb <- data %>%
#'   inti::fieldbook_design() %>%
#'   inti::fieldbook_varlist(varlist = varlist)
#'
#' fb$design
#'
#' }
#'
#' @export

fieldbook_varlist <- function(fieldbook
                              , varlist = NULL
                              , abbreviation = "abbreviation"
                              , evaluation = "evaluation"
                              , sampling = "sampling"
                              ) {

  blank <- Row.names <- varlits <- NULL

  abrv_match <- match.arg(abbreviation, c("abbreviation", "siglas"))
  eval_match <- match.arg(evaluation, c("evaluation", "eval", "dap", "dat"))
  smp_match <- match.arg(sampling, c("sampling", "sample", "subplot", "muestra"))

  data <- varlits %>%
    select( starts_with("{") |  ends_with("}") ) %>%
    rename_with(~ gsub("\\{|\\}", "", .)) %>%
    drop_na()

  if ( !all( c(abrv_match, eval_match, smp_match) %in% names(data) ) ) {

    return(fieldbook)

  }

  smp_n <- data[[smp_match]]

  var_list <- data %>%
    tidyr::uncount(smp_n, .id = "sample") %>%
    select(-sampling) %>%
    unite("var_list", {{abrv_match}}, {{eval_match}}, sample , sep = "_") %>%
    mutate(blank = NA) %>%
    pivot_wider(names_from = var_list, values_from = blank)

  fieldbook[["design"]] <- merge(fieldbook[["design"]], var_list
                         , by = c("row.names"), all.x = T) %>%
    select(-Row.names)

  fieldbook
}
