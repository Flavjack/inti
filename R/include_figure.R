#' Figure with caption and notes
#'
#' Include figures with title and notes using a data base
#'
#' @param data Data frame with the figures information. See details.
#' @param figure Name or path of the figure.
#' @param caption Manual figure caption (default = NA).
#' @param notes Manual figure notes (default = NA).
#' @param label Label for start the footnote (default = NA).
#' 
#' @details
#'
#' The data frame information result from `info_figure` output.
#'
#' @return Figure with caption and notes
#' 
#' @export
#' 
#' @examples 
#' 
#' library(inti)
#' 
#' fig <- info_figure(caption = "caption test."
#'                    , notes = "note test."
#'                    , label = "_Source:_"
#'                    , url = "https://devblackops.io/images/testing.jpg"
#'                    , path = "man/figures/logo.png"
#'                    )
#' 
#' # use this in r chunk `fig.caption = fig$caption`                   
#'                    
#' fig %>%  include_figure()
#' 

include_figure <- function(data = NULL
                         , figure
                         , caption = NA
                         , notes = NA
                         , label = NA
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
  
    if ( !is.na(path) && !is.na(url) && file.exists(path) ) {
      
      img_path <- path 
      
    } else if ( !is.na(url) ) {
      
      img_path <- url
    
    } else {
      
      stop("Cannot find the file")
      
    }
  
  img <- img_path %>% knitr::include_graphics()
  
# result ------------------------------------------------------------------
  
  img
  
}

