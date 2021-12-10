#' Google docs to Rmarkdown
#'
#' Use Articul8 Add-ons from Google docs to build Rticles
#'
#' @param file Zip file path from Articul8 exported in  md format
#' @param export path to export the files. Default file directory
#' @param prefix_fig Prefix for the name of the figure
#' @param prefix_tab Prefix for the name of the table
#'
#' @return folder
#' 
#' @export
#' 

gdoc2rmd <- function(file
                     , export = "files"
                     , prefix_fig = "Figure"
                     , prefix_tab = "Table"
                     ){
  
  zip <- file %>% 
    utils::unzip(overwrite = T, exdir = export)
  
  doc <- zip %>% 
    .[grep('.md', .)] %>%
    readLines() %>% 
    tibble::enframe() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = figure2rmd(.data$value, path = export, prefix = prefix_fig)) %>% 
    dplyr::mutate(value = table2rmd(.data$value, prefix = prefix_tab)) %>% 
    dplyr::select(.data$value) %>% 
    tibble::deframe() %>% 
    writeLines(con = paste0(export,"/_doc.Rmd"))
  
  doc <- list.files(path = export, pattern = "_doc", full.names = T)
  
}

