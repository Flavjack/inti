#' Figure with caption and notes
#'
#' Include figures with title and notes using a data base
#'
#' @param data data frame with the figures information. See details.
#' @param figure name of the figure in the fig_list.
#' @param fig_list column with the list of figures (default = "figures").
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

include_figure <- function(data = NULL
                           , figure
                           , caption = NULL
                           , notes = NULL
                           , label = "__Source:__"
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

  list(figure  = img, caption = cap)

}


source("http://lozanoisla.com/setup.r")
url <- "https://docs.google.com/spreadsheets/d/1R-uHCH5vZk9S2IlCzDOEg4rdi1ZxhPYxSoX5v_ohB5Y/edit#gid=13226945"
gs <- as_sheets_id(url)
# browseURL(url)

figure <- "zncl"

fig <- gs %>%
  range_read("fig") %>%
  include_figure(data = .
                 , figure = figure
                 , table = c("figures", "caption", "notes", "links")
                 )

fig$caption
fig$figure


img <- "https://lh3.googleusercontent.com/BqPAuF0wNISKAuI6xutzldsJdWNWY_geZ4DHaAkl-I_2ZIs--N-vn3jY8LKxjOFFyRcBvnmiQAx6uRWoAtyRIiU9tquXzusZ8eGZsPO8qkBAzBJqWgr6_9kpoRnx4RpPI1Z8GmLOVUDkSOUqPJuQg-cO5TUPGwQJBj4aFef_7wHxijEmO-MVHF-4CA3vExARiQPwl_DY6cQrPtJ0H3aqjc09WntxFKEfAWl-gkN7Z9FwUznxFek4428cXoaPc1R3nwoOFZECi0gpWq4jYzr0XGZF-yw_uLytA1Y_jPUn5DYcCHoFYn042Ek-f_oZSHhGjOKbkTFHHwP4R0VB85Mh7r5BxTtu165GLwUmfQwkg9ieScJiPZDjhhkEzwEa-TCQB5MM4vb6YRheQOIqBNzTVuuzN-J1HxoALZzQ1aakUheomhH9vokAibA7mZajqgNdoDspyw6leBdRG0IaZCEYt0tk7hCJM3cRG1yMtIfVxuRlqgRvoR5tzYz4ueeEBZNXcmth8TgqR0Ab0jp5_C5X9S60BHs2POt1x0XNssq_Qtk0ypNVvO9jRaf5MF_BRnwxQCJg2-BysNGCS6W6Hjhhw_p7ImaLDg-joY9Wlx3xyNGYIm8w8aD_zhnITtzVQ3BkjfBJ2lS61KGvWUEduSacqnggDkGLqr61VUo9XZNrJ97jTrjW69Eih2xJkn27ioby_Y6DIU1LrKWKzBTyeA1vanYjvaxy2sJDUdoZxpShNNFIhBS5Lg=w591-h475-no" %>%
  include_figure(figure = .
                 # , caption = "titulo de"
                 # , notes = "esto es"
                 )

img$figure
img$caption
