#' Table information
#'
#' Add table information for include_table
#'
#' @param data Data frame
#' @param caption Table caption.
#' @param notes Table notes.
#' @param label Note label.
#' @param notation Notation for the symbols and footnotes (default = "none").
#'   See details.
#'
#' @details
#'
#' You can add the footnote symbol using \code{{hypen}} in your table.
#' \code{notation} could be use: "alphabet", "number", "none".
#'
#' @return data frame
#'
#' @export
#'
#' @examples
#'
#' library(inti)
#'
#' tab <- data.frame(
#' x = rep_len(1, 5)
#' , y = rep_len(3, 5)
#' , z = rep_len("c", 5)
#' )
#'
#' info <- tab %>%
#' info_table(
#'   caption = "Figure caption"
#'   , notes = "test note"
#'   )
#' 

info_table <- function(data
                       , caption = NA
                       , notes = NA
                       , label = NA
                       , notation = "none"
                       ){
  
  # data <- tab
  
  tnames <- names(data) 
  
  col1 <- tnames[1] %>% as.name()
  col2 <- tnames[2] %>% as.name()
  
  tabinfo <- c("{caption}", "{notes}", "{label}", "{notation}") %>% 
    enframe(value = {{col1}}) %>% 
    select(!.data$name) %>% 
    tibble::add_column({{col2}} := c(caption, notes, label, notation))
  
  tab <- merge(data, tabinfo, all = TRUE, sort = F)
  
# results -----------------------------------------------------------------

list(info = tab, table = data)
  
}

