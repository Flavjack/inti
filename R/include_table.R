#' Table with footnotes
#'
#' Include tables with notes and titles for word and html documents
#'
#' @param data Data frame.
#' @param caption Table caption (default = NULL). See details.
#' @param notes Footnotes for the table (default = NULL). See details.
#' @param label Label for start the footnote (default = "Note:").
#' @param notation Notation for the symbols and footnotes (default =
#'   "alphabet"). See details.
#'
#' @details
#'
#' For \code{caption} and \code{notes} you can include {caption} and {notes} in
#' the last rows of your data frame. Where the information will be extracted
#' automatically for include in the formatted table. You can add the footnote
#' symbol using \code{{\-}} in your table. \code{notation} could be use:
#' "alphabet", "number", "none".
#'
#' @return Table with caption and footnotes
#'
#' @author Flavio Lozano-Isla
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
#'               , "1ilw0NHT7mihaM-3U48KzkuMt927xe8ukX6rNuIw2fT0/edit#gid=1157916926")
#'
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' table <- gs %>%
#'     range_read("footnotes")
#'
#' table %>% inti::include_table(label = "_Nota:_"
#'                               , notation = "num"
#' )
#'
#' }
#'

include_table <- function(data
                        , caption = NULL
                        , notes = NULL
                        , label = "Note:"
                        , notation = "alphabet"
) {

  # data <- table

  first_col <- names(data[1]) %>% as.symbol()
  col_list <- data[[first_col]]
  col_capt <- c("{caption}", "{title}", "{titulo}")
  col_note <- c("{notes}", "{note}", "{nota}", "{notas}")

  if(is.null(caption)){

    col_math <- col_list %in% col_capt
    col_cap <- col_list[col_math == TRUE]

    caption <- data %>%
      filter( {{first_col}} %in% {{col_cap}} ) %>%
      pluck(2)

  }

  if(is.null(notes)){

    col_math <- col_list %in% col_note
    col_note <- col_list[col_math == TRUE]

    if ( length(col_note) == 0 ) {

      note <- NULL

    } else {

      notes <- data %>%
        filter( {{first_col}}  == {{col_note}} ) %>%
        pluck(2) %>%
        as_vector()

    }

  }

  data %>%
    filter( !{{first_col}}  %in% c( {{col_capt}}, {{col_note}} ) ) %>%
    knitr::kable(caption = caption, format = "pipe") %>%
    inti::footnotes(notes = notes, label = label, notation = notation)

}
