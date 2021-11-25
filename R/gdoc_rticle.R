#' Google docs to Rmarkdown
#'
#' Use Articul8 Add-ons from Google docs to build a Rticles
#'
#' @param file Zip file path from Articul8 exported in  md format
#' @param export path to export the files. Default work directory
#' @param md Conserve md file. Default = TRUE
#'
#' @return folder
#' 
#' @export
#' 

gdoc_rticle <- function(file
                        , export = "files"
                        , md = TRUE
                        ){
  
  
# figure ------------------------------------------------------------------

  figure_md <- function(text
                        , path = export
                        , opts = NA
                        ) {
    
    opt <- text %>% 
      tibble::enframe() %>% 
      tidyr::separate_rows(.data$value, sep = "^(\\!\\[)") %>% 
      tidyr::separate_rows(.data$value, sep = "(\\]\\()") %>% 
      tidyr::separate_rows(.data$value, sep = "(\\)\\{)") %>% 
      tidyr::separate_rows(.data$value, sep = "(\\{)") %>% 
      tidyr::separate_rows(.data$value, sep = "(\\})") %>% 
      dplyr::na_if("") %>% 
      tidyr::drop_na(.data$value) %>% 
      tibble::rownames_to_column() %>% 
      dplyr::mutate(type = dplyr::case_when(
        grepl("img_", .data$value) ~ "figure"
        , grepl("#fig", .data$value) ~ "id"
        , rowname %in% 1 ~ "title"
        , TRUE ~ "options"
      )) %>% 
      dplyr::select(.data$type, .data$value) %>% 
      tibble::deframe() %>% 
      as.list()
    
    if(!is.na(opts)) {
      opt$options <- opts
    }
    
    fig <- paste0(
      "\n\n"
      , "```{r "
      , opt$options
      , ", fig.cap= '"
      , opt$title %>% trimws()
      , "'}\n\n"
      , "knitr::include_graphics('"
      , paste0(path, "/", opt$figure)
      , "')\n\n```"
    ) 
    
    fig
    
  }
  

# unzip files -------------------------------------------------------------

  zip <- file %>% 
    utils::unzip(overwrite = T, exdir = export)
  
  doc <- zip %>% 
    .[grep('.md', .)] %>%
    readLines() %>% 
    tibble::enframe() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(text = dplyr::case_when(
      grepl(pattern = "#fig:", value) ~ value %>% figure_md(path = file)
      , TRUE ~ value
    )) %>% 
    dplyr::select(.data$text) %>% 
    tibble::deframe() %>% 
    writeLines(con = paste0(export,"/_doc.Rmd"))
  
  doc <- list.files(path = export, pattern = "_doc", full.names = T)

}

