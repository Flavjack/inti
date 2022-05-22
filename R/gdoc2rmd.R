#' Google docs to Rmarkdown
#'
#' Use Articul8 Add-ons from Google docs to build Rticles
#'
#' @param file Zip file path from Articul8 exported in md format (path)
#' @param export Path to export the files (path: "docs")
#'
#' @return path
#' 
#' @export
#' 

gdoc2rmd <- function(file
                     , export = "docs"
                     ){
  
  zip <- file %>% 
    utils::unzip(overwrite = T, exdir = export)
  
  doc <- zip %>% 
    .[grep('.md', .)] %>%
    readLines() %>% 
    tibble::enframe() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = figure2rmd(.data$value, path = export)) %>% 
    dplyr::mutate(value = table2rmd(.data$value)) %>%
    dplyr::mutate(value = gsub("```Unknown element type at this position: UNSUPPORTED```", "\n", .data$value)) %>% 
    dplyr::select(.data$value) %>% 
    tibble::deframe() %>% 
    writeLines(con = file.path(export, "_doc.Rmd") %>% gsub("\\\\", "\\/", .))
  
  doc <- list.files(path = export, pattern = "_doc", full.names = T)
  
}

