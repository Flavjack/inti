#' Google docs to Rmarkdown
#'
#' Use Articul8 Add-ons from Google docs to build Rticles
#'
#' @param file Zip file path from Articul8 exported in md format [path]
#' @param export Path to export the files [path: NA (file directory)]
#'
#' @return path
#' 
#' @export
#' 

gdoc2qmd <- function(file
                     , export = NA
                     ){

  export <- if(is.na(export)) {
    file %>% gsub(".zip", "", .) %>% file.path()
  } else {export}
  
  zip <- file %>% 
    utils::unzip(overwrite = T, exdir = export)
  
  doc <- zip %>% 
    .[grep('.md', .)] %>%
    readLines() %>% 
    tibble::enframe() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = figure2qmd(.data$value, path = export)) %>% 
    dplyr::mutate(value = table2qmd(.data$value)) %>%
    dplyr::mutate(value = gsub("```Unknown element type at this position: UNSUPPORTED```", "\\\\newpage \n\n", .data$value)) %>% 
    dplyr::select(.data$value) %>% 
    tibble::deframe() %>% 
    writeLines(con = file.path(export, "_doc.Rmd") %>% gsub("\\\\", "\\/", .))
  
  doc <- list.files(path = export, pattern = "_doc", full.names = T)
  
}

