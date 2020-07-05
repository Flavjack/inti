#' Figure with caption and notes
#'
#' Include figures with title and notes using a data base
#'
#' @param data data frame with the figures information. See details.
#' @param figure name of the figure in the fig_list.
#' @param fig_list column with the list of figures (default = "figure").
#' @param path figure path or url (default = "url").
#' @param caption column with the figure caption (default = "caption").
#' @param notes column with the figure notes (default = "notes").
#' @param label label for start the footnote (default = "__Source:__").
#'
#' @details
#'
#' The data frame should contain 4 columns:
#' 1. \code{fig_list}
#' 2. \code{caption}
#' 3. \code{notes}
#' 4. \code{path} or \code{url}
#'
#' @return Figure with caption and notes
#'
#' @author
#'
#' Flavio Lozano-Isla
#'
#' @export

include_figure <- function(data
                           , figure
                           , fig_list = "figure"
                           , path = "url"
                           , caption = "caption"
                           , notes = "notes"
                           , label = "__Source:__"
){

  fig_list <- as.symbol(fig_list)

  path <- data %>%
    filter({{fig_list}} %in% {{figure}}) %>%
    select({{path}}) %>%
    as_vector()

  title <- data %>%
    filter({{fig_list}} %in% {{figure}}) %>%
    select({{caption}}) %>%
    as_vector()

  notes <- data %>%
    filter({{fig_list}} %in% {{figure}}) %>%
    select({{notes}}) %>%
    as_vector()

  fig <- path %>% knitr::include_graphics()

  if (is.na(notes)) {

    cap <- title

  } else {

    cap <- paste(title, {{label}}, "", notes)

  }

  list(
    figure = fig,
    caption = cap
  )
}
