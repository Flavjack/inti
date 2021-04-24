#' Fieldbook variable list
#'
#' Function to include the variables to evaluate in the fieldbook design.
#'
#' @param fieldbook Data frame with the fieldbook.
#' @param varlist Data frame with the variables information. See examples.
#'
#' @details The function allows to include the arguments in the sheet that have
#'   the information of the variables. You should include 3 columns in the
#'   sheet: \code{{abbreviation}}, \code{{evaluation}} and \code{{sampling}}.
#'   See examples. The information will be extracted automatically and deploy
#'   the list of variable for the fieldbook design. If you will be evaluate the
#'   experimental unit one time you should set 1.
#'
#' @return data frame
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' 
#' @export
#'

fieldbook_varlist <- function(fieldbook
                              , varlist = NULL
                              ) {

  var_list <- Row.names <- plots <- blank <- NULL

  if ( is.null(varlist) ) { return(fieldbook) }

  data <- varlist %>%
    dplyr::select( starts_with("{") |  ends_with("}") ) %>%
    select_if(~!all(is.na(.))) %>%
    dplyr::rename_with(~ gsub("\\{|\\}", "", .)) %>%
    drop_na()

# match names -------------------------------------------------------------
# -------------------------------------------------------------------------

  abrv_opt <- c("abbreviation", "sigla", "siglas", "abbrev", "variable", "var")
  eval_opt <- c("evaluation", "eval", "dap", "dat", "when", "cuando", "evaluar", "evaluate")
  smp_opt <- c("sampling", "sample", "samples"
               , "subplot", "subplots"
               , "muestra", "muestras")

  abrv_math <- names(data) %in% abrv_opt
  abrv_name <- names(data)[abrv_math == TRUE]

  eval_math <- names(data) %in% eval_opt
  eval_name <- names(data)[eval_math == TRUE]

  smp_math <- names(data) %in% smp_opt
  smp_name <- names(data)[smp_math == TRUE]

  if( length(abrv_name)  == 0 ) {

    return(fieldbook)

  }

# insert variables --------------------------------------------------------
# -------------------------------------------------------------------------

  if( length(abrv_name)  > 0 & length(eval_name)  > 0 & length(smp_name)  > 0 ) {

    smp_n <- data[[smp_name]]

    var_cols <- data %>%
      tidyr::uncount(smp_n, .id = {{smp_name}}) %>%
      dplyr::mutate(blank := NA) %>%
      unite("var_list", {{abrv_name}}, {{eval_name}}, {{smp_name}} , sep = "_") %>%
      distinct(var_list, .keep_all = TRUE) %>%
      pivot_wider(names_from = var_list, values_from = blank)

    fieldbook[["design"]] <- merge(fieldbook[["design"]], var_cols
                                   , by = c("row.names"), all.x = T) %>%
      dplyr::select(!Row.names) %>%
      dplyr::arrange(plots)

  }

  if( length(abrv_name)  > 0 & length(eval_name) == 0 & length(smp_name)  > 0 ) {

    smp_n <- data[[smp_name]]

    var_cols <- data %>%
      tidyr::uncount(smp_n, .id = {{smp_name}}) %>%
      dplyr::mutate(blank := NA) %>%
      unite("var_list", {{abrv_name}}, {{smp_name}} , sep = "_") %>%
      distinct(var_list, .keep_all = TRUE) %>%
      pivot_wider(names_from = var_list, values_from = blank)

    fieldbook[["design"]] <- merge(fieldbook[["design"]], var_cols
                                   , by = c("row.names"), all.x = T) %>%
      dplyr::select(!Row.names) %>%
      dplyr::arrange(plots)

  }

  if( length(abrv_name)  > 0 & length(eval_name) > 0 & length(smp_name)  == 0 ) {

    var_cols <- data %>%
      mutate(smp_name = 1) %>%
      unite( {{abrv_name}}, .data[[abrv_name]], .data[[eval_name]], smp_name , sep = "_") %>%
      dplyr::mutate(blank := NA) %>%
      distinct(.data[[abrv_name]], .keep_all = TRUE) %>%
      pivot_wider(names_from = .data[[abrv_name]], values_from = blank)

    fieldbook[["design"]] <- merge(fieldbook[["design"]], var_cols
                                   , by = c("row.names"), all.x = T) %>%
      dplyr::select(!Row.names) %>%
      dplyr::arrange(plots)

  }

  if( length(abrv_name)  > 0 & length(eval_name) == 0 & length(smp_name)  == 0 ) {

    var_cols <- data %>%
      mutate(smp_name = 1) %>%
      unite( {{abrv_name}}, .data[[abrv_name]], smp_name , sep = "_" ) %>%
      dplyr::mutate(blank := NA) %>%
      distinct(.data[[abrv_name]], .keep_all = TRUE) %>%
      pivot_wider(names_from = .data[[abrv_name]], values_from = blank)

    fieldbook[["design"]] <- merge(fieldbook[["design"]], var_cols
                                   , by = c("row.names"), all.x = T) %>%
      dplyr::select(!Row.names) %>%
      dplyr::arrange(plots)

  }

# result ------------------------------------------------------------------
# -------------------------------------------------------------------------

return(fieldbook)

}
