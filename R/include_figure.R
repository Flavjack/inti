#' Figure with caption and notes
#'
#' Include figures with title and notes using a data base
#'
#' @param data Data frame with the figures information. See details.
#' @param figure Name or path of the figure.
#' @param caption Manual figure caption (default = NA).
#' @param notes Manual figure notes (default = NA).
#' @param label Label for start the footnote (default = "Note:").
#' 
#' @details
#'
#' The data frame should contain 4 rows in \code{table}:
#'
#' 1. \code{caption}.
#'
#' 2. \code{notes}.
#'
#' 3. \code{url} 
#' 
#' 4. \code{path}.
#'
#' If you don't use a data frame you provide the information manually.
#'
#' @return Figure with caption and notes
#' 
#' @export
#' 
#' @examples 
#' 
#' library(googlesheets4)
#' library(inti)
#' 
#' finfo <- info_figure(caption = "test"
#'                    , notes = "nota"
#'                    , url = "https://devblackops.io/images/testing.jpg"
#'                    , path = "test.jpg"
#'                    )
#' 
#' fig <- finfo %>%  include_figure()
#' fig
#' 
#' 

include_figure <- function(data = NULL
                         , figure
                         , caption = NA
                         , notes = NA
                         , label = "Note:"
                         ){
  
  # data <- finfo
  
  data <- data %>% pluck(1)
  
  first_col <- names(data[1]) %>% as.symbol()
  col_list <- data[[first_col]]
  col_capt <- c("{caption}", "{title}", "{titulo}")
  col_note <- c("{notes}", "{note}", "{nota}", "{notas}")
  col_url <- c("{url}", "{link}")
  col_path <- c("{path}", "{dir}")
  

  if(!is.null(data)){
    
    col_math <- col_list %in% col_capt
    col_cap <- col_list[col_math == TRUE]
    
    caption <- data %>%
      filter( {{first_col}} %in% {{col_cap}} ) %>%
      purrr::pluck(2)
    
    col_math <- col_list %in% col_note
    col_note <- col_list[col_math == TRUE]
    
    notes <- data %>%
      filter( {{first_col}} %in% {{col_note}} ) %>%
      purrr::pluck(2)
    
    col_math <- col_list %in% col_url
    col_url <- col_list[col_math == TRUE]
    
    url <- data %>%
      filter( {{first_col}} %in% {{col_url}} ) %>%
      purrr::pluck(2)
    
    col_math <- col_list %in% col_path
    col_path <- col_list[col_math == TRUE]
    
    path <- data %>%
      filter( {{first_col}} %in% {{col_path}} ) %>%
      purrr::pluck(2)
    
  }
  
  if ( is.na(notes) ) { 
    
    cap <- caption
    
  } else if (!is.na(notes)) {
      
    cap <- paste(caption, label, notes)
    
    }
  
    if ( c(!is.na(path) && file.exists(path)) && !is.na(url) ) {
      
      img_path <- path 
      
    } else if ( !is.na(url) ) {
      
      img_path <- url
    
    } else {
      
      message("Include url or path for your figure")
      
    }
  
  img <- img_path %>% knitr::include_graphics()
  
# result ------------------------------------------------------------------
  
  img
  
}

