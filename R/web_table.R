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
#' @param scrolly Windows height to show the table. Default "45vh"
#' @param columnwidth Column width. Default '200px'
#' @param width Width in pixels or percentage (Defaults to automatic sizing)
#' 
#' @return table in markdown format for html documents
#'
#' @importFrom dplyr mutate across
#' @importFrom DT datatable
#' 
#' @export
#' @examples
#'
#' \dontrun{
#'
#' library(inti)
#' 
#' met %>%
#'   web_table(caption = "Web table")
#' 
#' }
#' 

web_table <- function(data
                      , caption = NULL
                      , digits = 2
                      , rnames = FALSE
                      , buttons = NULL
                      , file_name = "file"
                      , scrolly = "45vh"
                      , columnwidth = "200px"
                      , width = "100%"
){
  
  if(!is.data.frame(data)) stop("Use a data frame or table")
  
  where <- NULL
  
  if (is.null(buttons)) {
    
    ext <- c("Buttons", "Scroller")
    
  } else {
    
    ext <- c("Scroller")
    
  }
  
  botones <- list(
    list(extend = "copy"),
    list(extend = "excel", filename = file_name)
  )
  
  table <- data %>% 
    mutate(
      across(
        where(is.numeric),
        ~round(., digits = digits)
      )
    ) %>%
    datatable(
      extensions = ext,
      width = width,
      rownames = rnames,
      options = list(
        dom = "Bt",
        buttons = botones,
        deferRender = !is.null(scrolly),
        scroller = !is.null(scrolly),
        scrollX = TRUE,
        scrollY = scrolly,
        autoWidth = TRUE,
        columnDefs = list(
          list(
            width = columnwidth,
            targets = "_all"
          )
        ),
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this).addClass('compact');",
          "var container = $(this.api().table().container());",
          "container.find('thead th').css({'background-color': '#000', 'color': '#fff'});",
          "}"
        )
      ),
      caption = caption
    )
  
  table
}