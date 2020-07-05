#' Table with footnotes
#'
#' Include tables with notes and titles for word and html documents
#'
#' @param data data frame.
#' @param caption table caption. See details.
#' @param notes footnotes for the table. See details.
#' @param label label for start the footnote.
#' @param notation notation for the symbols and footnotes (default = "number"). See details.
#'
#' @details
#'
#' For \code{caption} and \code{notes} you can include [caption] and [notes] in the last rows of your data frame.
#' Where the information will be extracted automatically for include in the formatted table.
#' You can add the footnote symbol using \code{[fn]} in your table.
#' \code{notation} could be use: "alphabet", "number", "symbol", "none".
#'
#' @return Table with caption and footnotes
#'
#'
#' @export

include_table <- function(data
                        , caption = NULL
                        , notes = NULL
                        , label = "__Note:__"
                        , notation = "number"
) {

  first_col <- names(data[1]) %>%
    as.symbol()

  if(is.null(caption)){

    caption <- data %>%
      filter({{first_col}} == "[caption]") %>%
      pluck(2)
  }

  if(is.null(notes)){

    notes <- data %>%
      filter({{first_col}}  == "[notes]") %>%
      pluck(2) %>%
      as_vector()
  }

  data %>%
    filter(!{{first_col}}  %in% c("[notes]", "[caption]")) %>%
    knitr::kable(caption = caption, format = "pipe") %>%
    inti::footnotes(notes = notes, label = label, notation = notation)

}

