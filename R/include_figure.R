#' Figure with caption and notes
#'
#' Include figures with title and notes using a data base
#'
#' @param data data frame with the figures information. See details.
#' @param figure name or path of the figure.
#' @param caption manual figure caption (default = NULL).
#' @param notes manual figure notes (default = NULL).
#' @param label label for start the footnote (default = "Source:").
#' @param table columns in the table (default = c("figures", "caption", "notes", "url")).
#'
#' @details
#'
#' The data frame should contain 4 columns specified in \code{table}:
#' 1. \code{figures}.
#' 2. \code{caption}.
#' 3. \code{notes}.
#' 4. \code{path} or \code{url}.
#' If you don't use a a data frame you provide the information manually.
#'
#' @return Figure with caption and notes
#'
#' @author
#'
#' Flavio Lozano-Isla
#'
#' @export

include_figure <- function(data = NULL
                           , figure
                           , caption = NULL
                           , notes = NULL
                           , label = "Source:"
                           , table = c("figures", "caption", "notes", "url")
                           ){

  if(!is.null(data)) {

    fig_list <- as.symbol(table[1])
    cap_list <- as.symbol(table[2])
    not_list <- as.symbol(table[3])
    url_list <- as.symbol(table[4])

    title <- data %>%
      filter({{fig_list}} %in% {{figure}}) %>%
      select({{cap_list}}) %>%
      as_vector()

    notes <- data %>%
      filter({{fig_list}} %in% {{figure}}) %>%
      select({{not_list}}) %>%
      as_vector()

    path <- data %>%
      filter({{fig_list}} %in% {{figure}}) %>%
      select({{url_list}}) %>%
      as_vector()

    img <- path %>% knitr::include_graphics()

    if (is.na(notes)) {

      cap <- title

    } else {

      cap <- paste(title, {{label}}, "", notes)

    }

  } else if (is.null(data)) {

      img <- figure %>% knitr::include_graphics()

      if (is.null(caption) & is.null(notes)) {

        cap <- ""

      } else if (!is.null(caption) & !is.null(notes)){

        cap <- paste(caption, {{label}}, "", notes)

      } else if (!is.null(caption) & is.null(notes)){

        cap <- caption

      } else if (is.null(caption) & !is.null(notes)){

        message("You should include a caption")

        cap <- paste("", {{label}}, "", notes)

      }

  }

  list(figure = img, caption = cap)

}
