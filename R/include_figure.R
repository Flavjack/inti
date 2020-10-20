#' Figure with caption and notes
#'
#' Include figures with title and notes using a data base
#'
#' @param data Data frame with the figures information. See details.
#' @param figure Name or path of the figure.
#' @param caption Manual figure caption (default = NULL).
#' @param notes Manual figure notes (default = NULL).
#' @param label Label for start the footnote (default = "Source:").
#' @param table Columns in the table (default = c("figure", "description")).
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
#' if (gs4_has_token()) {
#' 
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "1dfgpmCKdPmxRHozrZp0iE_xMGsvKTIcztDpMWYSEGaY")
#' 
#' gs <- as_sheets_id(url)
#' 
#' data <- gs %>% 
#'   range_read("fig1")
#' 
#' fig <- include_figure(data)
#' fig
#' 
#' }
#' 

include_figure <- function(data = NULL
                         , figure
                         , caption = NULL
                         , notes = NULL
                         , label = "Note:"
                         , table = c("figures", "descripton")
                         ){
  
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
  
    if ( c(!is.na(path) & file.exists(path)) & !is.na(url) ) {
      
      img <- path %>% knitr::include_graphics()
      
    } else if (!is.na(url) ) {
      
      img <- url %>% knitr::include_graphics()
    
    } else {
      
      message("Include url or path for your figure")
      
    }

# result ------------------------------------------------------------------

  list(figure = img, caption = cap)

}

