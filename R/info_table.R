#' Table information
#'
#' Add table information for include_table
#'
#' @param data data frame
#' @param caption Figure caption.
#' @param notes Figure notes.
#'
#' @return data frame
#' 
#' @export
#' 
#' @examples 
#' 
#' tab <- tibble(
#' x = rep_len("a", 5)
#' , y = rep_len("b", 5)
#' , z = rep_len("c", 5)
#' )
#'
#' tinfo <- tab %>% 
#' info_table(
#'   caption = "Figure caption"
#'   , notes = "notes"
#'   )
#' 

info_table <- function(data
                        , caption = NA
                        , notes = NA
                        ){
  
  # data <- tab
  
  tnames <- names(data) 
  
  col1 <- tnames[1] %>% as.symbol()
  col2 <- tnames[2] %>% as.symbol()
  
  tabinfo <- c("{caption}", "{notes}") %>% 
    enframe(value = {{col1}}) %>% 
    select(!.data$name) %>% 
    tibble::add_column({{col2}} := c(caption, notes))
  
  
  tab <- bind_rows(data, tabinfo)
  

# results -----------------------------------------------------------------

tab  
  
}

