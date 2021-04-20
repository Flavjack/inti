#' HTML tables for markdown documents
#'
#' Export tables with download, pasta and copy buttons
#'
#' @param data Dataset.
#' @param digits Digits number in the table exported.
#' @param caption Title for the table.
#' @param rnames Row names.
#' @param buttons Buttons: "excel", "copy" or "none". Default c("excel", "copy")
#' @param file_name Excel file name
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
                      , file_name = NULL
                      ){
  
  if(!is.data.frame(data)) stop("Use a data frame or table")
  
  where <- NULL
  
  ext <- c('Buttons', 'Scroller')
  
  if (is.null(buttons)) {
    
    buttons <- list(
      list(extend = 'copy', filename = file_name)
      , list(extend = 'excel', filename = file_name)
    )
  }
  
  data %>% 
    mutate(across(where(is.numeric), ~round(., digits))) %>%
    datatable(extensions = ext
              , rownames = rnames
              , options = list(
                dom = 'Bt' # "Bti"
                , buttons = buttons
                , deferRender = TRUE
                , scroller = TRUE
                , scrollX = TRUE
                , scrollY = "60vh"
                
                , columnDefs = list(list(width = '200px'
                                         , targets = "_all"))
                
                , initComplete = DT::JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                  "}")
                )
              , caption = caption)

}
