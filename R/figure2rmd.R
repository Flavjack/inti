#' Figure to Rmarkdown
#'
#' Use Articul8 Add-ons from Google docs to build Rticles
#'
#' @param text String with the table information
#' @param path Path of the image fot the figure
#' @param opts chunk options in brackets. 
#' @param prefix Prefix for the name of the figure
#'
#' @return Mutated string
#' 
#' @export
#' 

figure2rmd <- function(text
                       , path = "."
                       , opts = NA
                       , prefix = "Figure"
                       ) {
  
  result <- if(isTRUE(grepl("^\\!\\[", text))) {
    
    opt <- text %>% 
      tibble::enframe() %>% 
      dplyr::mutate(info =  gsub("\\!\\[(.+)", "\\1", .data$value)) %>% 
      dplyr::mutate(title = gsub("(\\{|\\])(.*)\\}", "", .data$info) %>%  trimws()) %>% 
      dplyr::mutate(img =  regmatches(.data$info
                                      , gregexpr("img_[[:digit:]][[:punct:]][[:alpha:]]+"
                                                 , .data$info, perl=TRUE))) %>% 
      dplyr::mutate(chunk = regmatches(.data$info
                                       , gregexpr("\\{(.*)\\}\\]"
                                                  , .data$info, perl=TRUE)) %>% 
                      gsub("\\{|\\}|\\]", "",.)) %>% 
      
      dplyr::mutate(name =  regmatches(.data$info
                                       , gregexpr("\\{\\#fig\\:(.*)\\}"
                                                  , .data$info, perl=TRUE)) %>% 
                      gsub("\\{\\#fig\\:|\\}", "",.) %>% gsub("\\.", "", .)
      ) %>% 
      dplyr::select(!c(.data$value, .data$info)) %>% 
      dplyr::mutate(id := "figure") %>% 
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
      , ", fig.cap= '"
      , opt$title
      , "'}\n\n"
      , "knitr::include_graphics('"
      , paste0(path, "/", opt$img)
      , "')\n\n```"
    ) 
    
  } else if(isTRUE(grepl("\\[Figure", text))) {
    
    cite <- text %>% 
      regmatches(.
                 , gregexpr("\\[Figure(.+)\\]\\:"
                            , ., perl=TRUE)) %>% 
      regmatches(.
                 , gregexpr("\\@fig:(.+)[^\\]\\:]"
                            , ., perl=TRUE)) %>% 
      gsub("\\@", "\\@ref(", .) %>% 
      paste0(prefix, " \\\\", ., ")") %>% 
      gsub("\\.", "", .)
    
    
    parrafo <- text %>% 
      gsub("\\[Figure(.+)\\:", cite, .)
    
    
  } else {
    
    text
    
  }
  
  
  return(result)
  
}

