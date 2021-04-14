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
                      , digits = 2
                      , rnames = FALSE
                      , buttons = NULL
                      ){
  
  where <- NULL
  
  if (is.null(buttons)) {
    
    botones <- c("excel", "copy")
    ext <- c('Buttons', 'Scroller')
    
  } else {
    
    botones <- buttons
    ext <- c('Scroller')
  }
  
  data %>%
    mutate(across(where(is.numeric), ~round(., digits))) %>% 
    datatable(extensions = ext
              , rownames = rnames
              , options = list(dom = 'Bt'
                               , buttons = botones
                               , scroller = TRUE
                               , scrollX = TRUE
                               , scrollY = "60vh"
                               )
              , caption =  caption)

}
