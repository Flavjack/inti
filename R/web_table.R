#' HTML tables for markdown documents
#'
#' Export tables with download, pasta and copy buttons
#'
#' @param data Dataset.
#' @param digits Digits number in the table exported.
#' @param caption Title for the table.
#' @param rnames Row names.
#' @param buttons Buttons: "excel", "copy" or "none". Default c("excel", "copy")
#'
#' @return table in markdown format for html documents
#'
#' @importFrom dplyr mutate across
#' @importFrom DT datatable
#' 
#' @export
#' 

web_table <- function(data
                      , caption = NULL
                      , digits = 3
                      , rnames = FALSE
                      , buttons = NULL
                      ){
  
  where <- NULL
  
  data <- data %>%
    mutate(across(where(is.numeric), ~round(., digits))) 

  if (is.null(buttons)){

    data %>% 
      datatable(extensions = c('Buttons', 'Scroller'),
                rownames = rnames,
                options = list(dom = 'Bt',
                               buttons = c("excel", "copy"),
                               autoWidth = TRUE, scroller = TRUE,
                               scrollY = "50vh", scrollX = TRUE),
                caption =  caption)

  } else if (buttons == "none"){

    data %>% 
      datatable(extensions = c('Scroller'),
                rownames = rnames,
                options = list(dom = 'Bt',
                               buttons = buttons,
                               autoWidth = TRUE, scroller = TRUE,
                               scrollY = "50vh", scrollX = TRUE),
                caption =  caption)

  } else {

    data %>%
      datatable(extensions = c('Buttons','Scroller'),
                rownames = rnames,
                options = list(dom = 'Bt',
                               buttons = buttons,
                               autoWidth = TRUE, scroller = TRUE,
                               scrollY = "50vh", scrollX = TRUE),
                caption =  caption)

  }

}
