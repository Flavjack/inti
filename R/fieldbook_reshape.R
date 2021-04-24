#' Fieldbook reshape
#'
#' Function to reshape fieldbook according a separation character
#'
#' @param data Field book raw data.
#' @param last_factor The last factor in your field book.
#' @param sep Character that separates the last value.
#' @param new_colname The new name for the column created.
#' @param from_var The first variable in case you want to exclude several.
#'   variables.
#' @param to_var The last variable in case you want to exclude several
#'   variables.
#' @param exc_factors Factor to exclude during the reshape.
#'
#' @details
#'
#' If you variable name is \code{variable_evaluation_rep}. The reshape function
#' will help to create the column \code{rep} and the new variable name will be
#' \code{variable_evaluation}.
#'
#' @return data frame
#'
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider pivot_longer
#'
#' @export
#' 

fieldbook_reshape <- function(data
                              , last_factor
                              , sep
                              , new_colname
                              , from_var = NULL
                              , to_var = NULL
                              , exc_factors = NULL
                             ) {

  where <- NULL

  if ( is.null(from_var) || from_var == "") {

    from_var <- 1

  } else { from_var <- from_var }

  if ( is.null(to_var) || to_var == "" ) {

    to_var <- ncol(data)

  } else { to_var <- to_var }

  fb <- data %>%
    select(1:{{last_factor}}, {{from_var}}:{{to_var}}) %>%
    select(where(~!all(is.na(.)))) %>%
    {if (!is.null(exc_factors)) select(., !{{exc_factors}} ) else .}  %>%
    pivot_longer(cols = !c(1:{{last_factor}}),
                 names_to = c("variables", {{new_colname}}),
                 names_sep = paste0("_(?!.*",{{sep}},")"),
                 values_to = c("values")) %>%
    pivot_wider(names_from = .data$variables, values_from = .data$values)

# result ------------------------------------------------------------------

  return(fb)

}
