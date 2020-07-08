#' HTML tables for markdown documents
#'
#' Export tables with download, pasta and copy buttons
#'
#' @param data dataset
#' @param digits digits number in the table exported
#' @param caption Title for the table
#' @param rnames row names
#' @param buttons "excel", "copy" or "none". Default c("excel", "copy")
#'
#' @return table in markdown format for html documents
#'
#' @author Flavio Lozano-Isla
#'
#' @importFrom dplyr mutate_if
#' @importFrom DT datatable
#' @export

web_table <- function(data, caption = NULL, digits = 3, rnames = FALSE, buttons = NULL){

  if (is.null(buttons)){

    data %>%
      mutate_if(is.numeric, ~round(., digits)) %>%
      datatable(extensions = c('Buttons', 'Scroller'),
                rownames = rnames,
                options = list(dom = 'Bt',
                               buttons = c("excel", "copy"),
                               autoWidth = TRUE, scroller = TRUE,
                               scrollY = "50vh", scrollX = TRUE),
                caption =  caption)

  } else if (buttons == "none"){

    data %>%
      mutate_if(is.numeric, ~round(., digits)) %>%
      datatable(extensions = c('Scroller'),
                rownames = rnames,
                options = list(dom = 'Bt',
                               buttons = buttons,
                               autoWidth = TRUE, scroller = TRUE,
                               scrollY = "50vh", scrollX = TRUE),
                caption =  caption)

  } else {

    data %>%
      mutate_if(is.numeric, ~round(., digits)) %>%
      datatable(extensions = c('Buttons','Scroller'),
                rownames = rnames,
                options = list(dom = 'Bt',
                               buttons = buttons,
                               autoWidth = TRUE, scroller = TRUE,
                               scrollY = "50vh", scrollX = TRUE),
                caption =  caption)

  }

}
