#' Transform data frame based in a dictionary
#'
#' Transform entire fieldbook according to data a dictionary
#'
#' @param fieldbook Data frame with the original information.
#' @param dictionary Data frame with new names and categories. See details.
#' @param from Column of the dictionary with the original names.
#' @param to Column of the dictionary with the new names.
#' @param index Column of the dictionary with the type and level of the
#'   variables.
#' @param colnames Character vector with the name of the columns.
#'
#' @details 
#' 
#' The function require at least three columns.
#'
#'   1. Original names (\code{from}).
#'
#'   2. New names (\code{to}).
#'
#'   3. Variable type (\code{index}).
#'
#' @return List with two objects. 1. New data frame. 2. Dictionary.
#'
#' @author Flavio Lozano-Isla
#'
#' @importFrom dplyr mutate_all starts_with sym vars filter rename_at mutate_at
#' @importFrom purrr as_vector
#' @importFrom tidyr drop_na
#' @importFrom tibble as_tibble
#' @importFrom graphics hist
#' @importFrom stats qqline qqnorm resid sd vcov
#' 
#' @export
#' 
#' @examples
#'
#' \dontrun{
#'
#' library(googlesheets4)
#' library(tidyverse)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#' , "1gue-wSQcEu4nJigVZdUWsTfIIzhtxpDRdWAQiEHgKak/edit#gid=0")
#' gs <- as_sheets_id(url)
#' # browseURL(url)
#'
#' fb_old <- gs %>%
#'   sheets_read("fb_old") %>%
#'   select(ID:SNum) %>%
#'   filter(SS == "S1")
#'
#' dic <- gs %>% sheets_read("var_old") %>%
#'   slice(1:20)
#'
#' mtm <- metamorphosis(fieldbook = fb_old
#'                         , dictionary = dic
#'                         , from = "old_name"
#'                         , to = "Abbreviation"
#'                         , index = "Type"
#'                         , colnames = c("colname")
#'                      )
#'
#' fb_new <- mtm$fieldbook
#'
#' }
#'

metamorphosis <- function(fieldbook
                          , dictionary
                          , from
                          , to
                          , index
                          , colnames
                          ){

  # Import dictionary -------------------------------------------------

  dictionary_used <- dictionary %>%
    tidyr::drop_na(from) %>%
    dplyr::mutate_all(as.character)

  # column names ------------------------------------------------------------

  rename_colums <- function(fieldbook, dictionary, from, to, index, colnames) {


    cln <- dictionary %>%
      dplyr::filter(!!sym(index) %in% colnames) %>%
      dplyr::select(from, to) %>%
      tidyr::drop_na(from)

    old_names <- cln %>%
      dplyr::select(from) %>%
      purrr::as_vector()

    new_names <- cln %>%
      dplyr::select(to) %>%
      purrr::as_vector()

    fbr <- fieldbook %>%
      dplyr::rename_at(vars(old_names), ~ new_names)

    fbr

  }

  fb_renamed <- rename_colums(fieldbook, dictionary_used, from, to, index, colnames)

  # Recode the variable levels ----------------------------------------------

  rename_levels <- function(fb_renamed, dictionary, from, to, index, colnames, variable){

    # variable levels

    vrl <- dictionary %>%
      dplyr::filter(!(!!sym(index)) %in% colnames)


    # Check if variable exist

    colums_to_recode <- dictionary %>%
      dplyr::filter(!(!!sym(index)) %in% colnames) %>%
      dplyr::select(index) %>%
      base::unique() %>%
      purrr::as_vector()


    if(is.element(variable, colums_to_recode) == FALSE){

      rnf <- fb_renamed %>%
        dplyr::select(variable)

      rnf

    } else {

      # Old variable names

      old_v <-  vrl  %>%
        dplyr::filter(!!sym(index) == variable) %>%
        dplyr::select(from) %>%
        purrr::as_vector()

      # New variable names

      new_v <-  vrl %>%
        dplyr::filter(!!sym(index) == variable) %>%
        dplyr::select(to) %>%
        purrr::as_vector()

      # Lista variables to recode

      rnm <- structure(as.character(new_v),
                       names = as.character(old_v))

      # Recode one variable

      rnf <- fb_renamed %>%
        dplyr::mutate_at(variable, list(~recode(., !!!rnm))) %>%
        dplyr::select(variable)

      rnf

    }


  }

  # Recode all the variables in the data frame

  fb_recoded <- lapply(1:ncol(fb_renamed), function(x) {

    fb_renamed %>%
      rename_levels(., dictionary_used, from, to, index, colnames, variable = colnames(.)[x])

  })

  # Unite the lists ---------------------------------------------------------

  fb_recoded <- do.call(cbind, fb_recoded) %>%
    tibble::as_tibble()


  # Extract used dictionary -------------------------------------------------

  vrl <- dictionary %>%
    tidyr::drop_na(from) %>%
    dplyr::filter(!!sym(index) %in% colnames) %>%
    dplyr::select(to) %>%
    base::unique() %>%
    unlist() %>%
    purrr::as_vector()


  dictionary_all <- dictionary %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::filter(!!sym(to) %in% vrl | !!sym(index) %in% vrl)

  # Result ------------------------------------------------------------------

  list(
    dictionary = dictionary_all,
    fieldbook = fb_recoded
  )

}
