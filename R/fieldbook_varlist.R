#' Field book variable list
#'
#' Function to include the variables to evaluate in the fieldbook design.
#'
#' @param fieldbook Data frame with the fieldbook.
#' @param varlist Data frame with the variables information. See examples.
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
#' , "1a82XIbTeWPC4pwvu9Zjl4qrGitGQ_B-mf3-w67VS4-Q/edit#gid=0")
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (design <- gs %>%
#'     range_read("design"))
#'
#' (varlist <- gs %>%
#'     range_read("traits"))
#'
#' fieldbook <- design %>%
#'   inti::fieldbook_design() %>%
#'   inti::fieldbook_varlist(varlist)
#'
#' fieldbook
#'
#' }
#'
#' @export

fieldbook_varlist <- function(fieldbook
                              , varlist = NULL
                              ) {

  var_list <- Row.names <- plots <- blank <- NULL

  if ( is.null(varlist) ) { return(fieldbook) }

  data <- varlist %>%
    dplyr::select( starts_with("{") |  ends_with("}") ) %>%
    dplyr::rename_with(~ gsub("\\{|\\}", "", .)) %>%
    drop_na()

# match names -------------------------------------------------------------
# -------------------------------------------------------------------------

  abrv_opt <- c("abbreviation", "siglas")
  eval_opt <- c("evaluation", "eval", "dap", "dat")
  smp_opt <- c("sampling", "sample", "subplot", "muestra")

  abrv_math <- names(data) %in% abrv_opt
  abrv_name <- names(data)[abrv_math == TRUE]

  eval_math <- names(data) %in% eval_opt
  eval_name <- names(data)[eval_math == TRUE]

  smp_math <- names(data) %in% smp_opt
  smp_name <- names(data)[smp_math == TRUE]

  if(length(abrv_name)  == 0 |  length(eval_name)  == 0 |  length(smp_name) == 0) {

    return(fieldbook)

  }

# insert variables --------------------------------------------------------
# -------------------------------------------------------------------------

  smp_n <- data[[smp_name]]

  var_cols <- data %>%
    tidyr::uncount(smp_n, .id = {{smp_name}}) %>%
    dplyr::mutate(blank := NA) %>%
    unite("var_list", {{abrv_name}}, {{eval_name}}, {{smp_name}} , sep = "_") %>%
    pivot_wider(names_from = var_list, values_from = blank)

  fieldbook[["design"]] <- merge(fieldbook[["design"]], var_cols
                         , by = c("row.names"), all.x = T) %>%
    dplyr::select(!Row.names) %>%
    dplyr::arrange(plots)

  fieldbook
}
