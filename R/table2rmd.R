#' Table to Rmarkdown format
#'
#' Use Articul8 Add-ons from Google docs to build Rticles
#'
#' @param text String with the table information
#' @param opts chunk options in brackets. 
#'
#' @return Mutated string
#' 
#' @export
#' 

table2rmd <- function(text
                      , opts = NA
                      ) {
  
  result <- if(grepl("\\{\\#tbl", text)) {
    
    opt <- text %>% 
      tibble::enframe() %>% 
      dplyr::mutate(title =  regmatches(.data$value
                                        , gregexpr("\\:(.+)\\{"
                                                   , .data$value, perl=TRUE)) %>% 
                      gsub("\\:|\\{", "", .) %>% trimws()
      ) %>% 
      dplyr::mutate(chunk = regmatches(.data$value
                                       , gregexpr("\\{(.*)\\}\\]"
                                                  , .data$value, perl=TRUE)) %>% 
                      gsub("\\{|\\}|\\]", "",.)) %>% 
      dplyr::mutate(name = regmatches(.data$value
                                      , gregexpr("\\{\\#tbl\\:(.*)\\}"
                                                 , .data$value, perl=TRUE)) %>% 
                      gsub("\\{\\#tbl\\:|\\}", "",.) %>% gsub("\\.", "", .)
      ) %>% 
      dplyr::select(!c(.data$value)) %>% 
      dplyr::mutate(id := "table") %>% 
      dplyr::mutate(across(everything(), as.character)) %>% 
      tidyr::pivot_longer(!.data$id) %>% 
      dplyr::select(!.data$id) %>% 
      dplyr::na_if("character(0)") %>% 
      tibble::deframe() %>% 
      as.list()
    
    chunk_opts <- if(!is.na(opts)) { opts 
    } else if (is.na(opt$chunk)) { opt$name
    } else { paste(opt$name, opt$chunk, sep = ", ")}
    
    chunk <- paste0(
      "\n\n"
      , "```{r "
      , chunk_opts
      , "}\n\n"
      , "knitr::kable(NA, col.names = NULL, caption ="
      , "'", opt$title, "')"
      , "\n\n```"
    ) 
    
  } else if(isTRUE(grepl("\\[Table", text))) {
    
    cite <- text %>% 
      regmatches(.
                 , gregexpr("\\[Table(.+)\\]\\:"
                            , ., perl=TRUE)) %>% 
      regmatches(.
                 , gregexpr("\\@tbl:(.+)[^\\]\\:\\)]"
                            , ., perl=TRUE)) %>% 
      gsub("\\@tbl", "\\@ref(tab", .) %>% 
      paste0("Table", " \\\\", ., ")") %>% # prefix ==> "Table"
      gsub("\\.", "", .)
    
    
    parrafo <- text %>% 
      gsub("\\[Table(.+)\\:", cite, .)
    
  } else {
    
    text
    
  }
  
  return(result)
  
}