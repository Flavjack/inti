#' Fieldbook variable list
#'
#' Function to include the variables to evaluate in the fieldbook design.
#'
#' @param fieldbook Data frame with the fieldbook.
#' @param varlist Data frame with the variables information. See examples.
#'
#' @details The function allows to include the arguments in the sheet that have
#'   the information of the variables. You should include 3 columns in the
#'   sheet: \code{{abbreviation}}, \code{{evaluation}} and \code{{sampling}}.
#'   See examples. The information will be extracted automatically and deploy
#'   the list of variable for the fieldbook design.
#'
#' @return data frame
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider unite
#' 
#' @export
#' @examples
#'
#' \dontrun{
#' 
#' library(inti)
#' library(gsheet)
#' 
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "183upHd4wriZz2UnInoo5Ate5YFdk7cZlhE0sMQ2x5iw/edit#gid=532773890")
#' # browseURL(url)
#' 
#' info <- gsheet2tbl(url) 
#'
#' fieldbook <- tarpuy_design(data = info)
#' 
#' url_var <- paste0("https://docs.google.com/spreadsheets/d/"
#'        , "183upHd4wriZz2UnInoo5Ate5YFdk7cZlhE0sMQ2x5iw/edit#gid=1335288687")
#'        
#' varlist <- gsheet2tbl(url_var) 
#' 
#' tarpuy_varlist(fieldbook = fieldbook, varlist = varlist)
#' 
#' }
#' 

tarpuy_varlist <- function(fieldbook
                              , varlist = NULL
                              ) {
  

  # varlist <- variables
  # fieldbook <- dsg
  
# -------------------------------------------------------------------------
  
  where <- NULL
  
# -------------------------------------------------------------------------
  
  if ( is.null(varlist) ) { return(fieldbook) }
  
# -------------------------------------------------------------------------
 
  vartable <- varlist %>%
    dplyr::select( starts_with("{") |  ends_with("}") ) %>%
    select(where(~!all(is.na(.)))) %>% 
    dplyr::rename_with(~ gsub("\\{|\\}", "", .))
  
# -------------------------------------------------------------------------
  
  if( nrow(vartable)  == 0 | ncol(vartable)  == 0 ) {
    
    return(fieldbook)
    
  }
  
# -------------------------------------------------------------------------
  smp_opt <- c("sampling", "sample", "samples"
               , "subplot", "subplots"
               , "muestra", "muestras")
  
  smp_math <- names(vartable) %in% smp_opt
  sampling <- names(vartable)[smp_math == TRUE]
  
  traits <- vartable %>% 
    { if(length(sampling) > 0)
    dplyr::mutate(.data = ., {{sampling}} := case_when(
      is.na( .data[[sampling]] ) ~ 1
      , TRUE ~ .data[[sampling]]
    )) %>% 
        tidyr::uncount(.data[[sampling]], .id = {{sampling}})
      else . 
      } %>% 
    dplyr::mutate(across(everything(), as.character)) %>% 
    dplyr::rowwise() %>%
    dplyr::mutate("trait" := paste(across(where(is.character))
                                       , collapse = "_")) %>% 
    dplyr::select(.data$trait) %>% 
    dplyr::mutate("blank" := NA) %>% 
    tidyr::pivot_wider(names_from = .data$trait, values_from = .data$blank)
  
  fb <- merge(fieldbook
              , traits
              , by = c("row.names")
              , all.x = T
              ) %>%
    dplyr::select(!.data$Row.names) %>%
    dplyr::arrange(.data$plots)
    
# -------------------------------------------------------------------------
  
  return(fb)
  
}
